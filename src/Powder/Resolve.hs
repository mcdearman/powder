{-# LANGUAGE ViewPatterns #-}

-- | Syntax to HIR: a little desugaring, and name resolution.
--
-- The desugaring is what needs nothing but the syntax and the fixities:
-- operators become applications, grouped by precedence; sections, negation,
-- ranges and @do@ become lambdas and applications of ordinary names;
-- tuples and lists become constructors; parentheses go; and the equations
-- of a function are gathered into one binding.
--
-- Name resolution gives every binder an identity of its own and replaces
-- every use of a name with the identity of what it refers to. After it
-- there are no scopes to think about: a name that is not in scope has been
-- reported, and one that is cannot be confused with another spelled the
-- same.
--
-- Every node made is given an identity too, and where it came from is
-- noted under it. A mistake does not stop the pass. It is reported, an
-- error node stands in for what was wrong, and the rest is still resolved,
-- so that everything wrong with a program is found at once.
module Powder.Resolve (resolve, resolveAfter, Seed (..), Linkage (..), Scope (..)) where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IntMap
import Data.List (elemIndex, intercalate, isSuffixOf, nub, (\\))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Char (isAlphaNum, isUpper)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Ratio (denominator, numerator)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric (readFloat)
import Powder.Core (ConInfo (..), Name, Prim (Coerce, MatchFail), primName)
import Powder.Extensions (Extension (..), Extensions, reportPrelude)
import Powder.Diagnostics (Problem (..))
import Powder.HIR hiding (mine)
import qualified Powder.HIR.Pat as Pat
import qualified Powder.HIR.Type as Type
import Powder.Parser (Parsed (..), parseModule, parseModuleWith)
import Powder.Syntax.AST (Ast, Module, cast, moduleDecls, moduleHeader, operatorName, syntax)
import qualified Powder.Syntax.AST as Syntax
import Powder.Syntax.AST.Decl (Assoc (..))
import qualified Powder.Syntax.AST.Decl as Decl
import qualified Powder.Syntax.AST.Expr as Expr
import qualified Powder.Syntax.AST.Pat as SPat
import qualified Powder.Syntax.AST.Stmt as Stmt
import qualified Powder.Syntax.AST.Type as SType
import qualified Powder.Syntax.Kind as Kind
import qualified Powder.Syntax.Red as Red

-- | What is in scope.
data Env = Env
  { -- | Variables, innermost binding first by construction: a local name
    -- replaces a global of the same spelling.
    envValues :: Map.Map Name NameId,
    -- | The top level alone, for what the desugaring refers to.
    envGlobals :: Map.Map Name NameId,
    envConstructors :: Map.Map Name NameId,
    -- | Type constructors and synonyms, and the type variables in scope.
    envTypes :: Map.Map Name NameId,
    envTypeVariables :: Map.Map Name NameId,
    envClasses :: Map.Map Name NameId,
    envFile :: FilePath,
    -- | Where the syntax being resolved is, for what is made from it.
    envOrigin :: Maybe Origin,
    -- | The module being resolved, and the one whose names are spelled
    -- plainly in what is compiled.
    envModule :: Name,
    envRoot :: Name,
    -- | The names that more than one thing in scope goes by.
    envAmbiguous :: Set.Set Name,
    -- | The module's own names, for where one is defined: there it is
    -- that one, whatever else goes by the name.
    envOwnValues :: Map.Map Name NameId,
    envOwnConstructors :: Map.Map Name NameId,
    -- | What the module has at its top by each name, whatever is bound
    -- nearer: a field's name is looked up here, where a variable of the
    -- same name does not hide it.
    envTop :: Map.Map Name NameId,
    -- | Everything the module has at its top by each name, where there
    -- may be more than one: a field is told from another of its name by
    -- the constructor it is used with.
    envTopAll :: Map.Map Name [NameId],
    -- | The extensions each file is written with.
    envLanguages :: Map.Map FilePath Extensions,
    -- | Whether what is being resolved was made up here, so that it is
    -- all placed where what it was made up for is.
    envPinned :: Bool,
    -- | The classes of what the prelude is made from, as 'envGlobals' is
    -- its values: what is made up here is an instance of some.
    envGlobalClasses :: Map.Map Name NameId,
    -- | What the module GHC.Generics lets out, if there is one: an
    -- instance of Generic derived here is written with it.
    envGenerics :: Scope
  }

data St = St
  { nextId :: Int,
    names :: Table NameInfo,
    origins :: Table Origin,
    signatures :: Table Signature,
    synonyms :: Table Synonym,
    -- | Most recent first.
    dataTypes :: [DataType],
    classes :: Table Class,
    -- | Most recent first.
    instances :: [Instance],
    -- | For each field, the constructors that have it and where in each.
    fields :: Table [(NameId, Int)],
    -- | Definitions made here and not written: a field's function.
    generated :: [Binding],
    defaults :: [(FilePath, [Type.Type])],
    -- | The fixity each operator was declared to have, by 'NameId'.
    fixityTable :: Table (Assoc, Int),
    newtypes :: [NameId],
    -- | Which of each constructor's fields are strict, by 'NameId'.
    strict :: Table [Bool],
    problems :: [Problem],
    -- | What the view patterns met since they were last taken ask for:
    -- for each, that its function applied to what was matched matches
    -- its pattern.
    views :: [Qualifier],
    -- | The names of what the strict bindings met since they were last
    -- taken bind, each to be worked out before what the bindings are for.
    forced :: [NameId],
    -- | For the variable each use of a pattern synonym in a pattern
    -- became: the function that builds the synonym, if it has one, and
    -- the patterns the synonym was applied to. It is how such a pattern
    -- is read back as an expression.
    synonymUses :: [(NameId, (Maybe NameId, [Pat]))],
    -- | The types names were said to have with parts left out, by 'NameId'.
    partials :: Table Signature,
    -- | For a name whose signature names its type variables with a
    -- forall, where they are in scope in its definition: the variables.
    scopedVariables :: Table [(Name, NameId)],
    -- | The parts left out of the type being read: a variable for each.
    wildcards :: [NameId],
    -- | The kinds type variables and types were said to have, by 'NameId',
    -- and the types said to have one where they are written.
    kindsSaid :: Table KindSyntax,
    kindedTypes :: [(Type.Type, KindSyntax)],
    -- | The names constraints were given, by 'NameId': each its
    -- parameters and the constraints it stands for.
    constraintSynonyms :: Table ([NameId], [Constraint]),
    -- | The functions from types to a type, by 'NameId'.
    families :: Table Family,
    -- | The numbers and strings met as types, each as it is shown, and
    -- the type each is.
    literalTypes :: Map.Map String NameId,
    -- | For a method whose default has a type of its own, by 'NameId':
    -- the function that default is.
    defaultSigned :: Table NameId,
    -- | The numbers and strings met as types that have no instance yet
    -- of the class that says which they are: met before the class was.
    literalsPending :: [(NameId, Literal, Name)]
  }

type M = ReaderT Env (State St)

-- | Resolve a list of modules, each with the name of its file, into one
-- program, with an expression to bind to a name of its own if one is
-- given: what a prompt was asked to evaluate. With it may be a second
-- name, to bind to the action that prints the expression's value.
--
-- The sources are a package: every module there is to know about. Each
-- comes with the name of its module, and several may be parts of one
-- module, as what is typed at a prompt is; then a definition in a later
-- part replaces one of the same name in an earlier part, for the earlier
-- part's uses of it too. The names in scope in a module are its own and
-- those it imports, and what a module exports may depend on what it
-- imports, so that modules that import each other, which they may, are
-- settled together: every module's exports start empty and are worked out
-- again from the others' until none changes.
--
-- In the modules named first, a definition of a name hides an imported
-- one of the same name, where otherwise a use of it would be ambiguous:
-- that is for a prompt, where defining again is what one does.
resolve :: Map.Map FilePath Extensions -> [Name] -> [(FilePath, Name, Module)] -> Maybe (Name, Maybe Name, FilePath, Syntax.Expr) -> Either [Problem] Program
resolve languages lenient sources extra = fst <$> resolveAfter Nothing languages lenient sources extra

-- | What name resolution knows that a program does not say: what each
-- module lets out, and what the resolving of later modules looks up.
-- With a program's tables it is all a package compiled later needs of
-- one compiled before.
data Linkage = Linkage
  { -- | What each module exports.
    linkExports :: Map.Map Name Scope,
    -- | Everything the module the prelude is made from defines, which is
    -- what the desugaring refers to.
    linkBase :: Maybe Scope,
    -- | What belongs to each type and class: constructors, and fields or
    -- methods.
    linkBelongings :: Table ([(Name, NameId)], [(Name, NameId)]),
    -- | The types and constructors the language has, and what it refers
    -- to itself.
    linkWired :: Maybe ([(Name, NameId)], [(Name, NameId)], Builtins),
    linkFields :: Table [(NameId, Int)],
    linkFixities :: Table (Assoc, Int),
    linkStrict :: Table [Bool],
    -- | For a method whose default has a type of its own: the function
    -- that default is.
    linkDefaults :: Table NameId
  }

instance Semigroup Linkage where
  Linkage a b c d e f g h <> Linkage a' b' c' d' e' f' g' h' =
    Linkage (Map.union a a') (b <|> b') (IntMap.union c c') (d <|> d') (IntMap.union e e') (IntMap.union f f') (IntMap.union g g') (IntMap.union h h')

instance Monoid Linkage where
  mempty = Linkage Map.empty Nothing IntMap.empty Nothing IntMap.empty IntMap.empty IntMap.empty IntMap.empty

-- | The packages a package is compiled after: where its identities start,
-- and what is known of the others. The lists are in the order of a
-- program's.
data Seed = Seed
  { seedFloor :: Int,
    seedLinkage :: Linkage,
    seedNames :: Table NameInfo,
    seedOrigins :: Table Origin,
    seedSignatures :: Table Signature,
    seedSynonyms :: Table Synonym,
    seedDataTypes :: [DataType],
    seedClasses :: Table Class,
    seedInstances :: [Instance],
    seedNewtypes :: [NameId],
    seedModules :: [(FilePath, String)],
    seedImports :: [(String, [String])],
    -- | The kinds the packages before said types and their variables
    -- have, and their functions from types.
    seedKinds :: Table KindSyntax,
    seedFamilies :: Table Family
  }

-- | Resolve modules as 'resolve' does, as a package compiled after
-- others if a seed is given. Then the program has the others' types,
-- classes and instances in it beside its own, but only its own
-- definitions; no module's names are spelled plainly; and a module of the
-- others can be imported. With the program comes what a package compiled
-- after this one will need to be given in its turn.
resolveAfter :: Maybe Seed -> Map.Map FilePath Extensions -> [Name] -> [(FilePath, Name, Module)] -> Maybe (Name, Maybe Name, FilePath, Syntax.Expr) -> Either [Problem] (Program, Linkage)
resolveAfter seed languages lenient sources extra = case reverse (problems final) of
  [] -> Right (program, linked {linkFields = fields final, linkFixities = fixityTable final, linkStrict = strict final, linkDefaults = defaultSigned final})
  found -> Left found
  where
    program = Program (bindings ++ reverse (generated final)) (names final) (origins final) (signatures final) (reverse (dataTypes final)) (synonyms final) (classes final) (reverse (instances final)) builtins spelled (defaults final) (newtypes final) (maybe [] seedModules seed ++ [(file, n) | (file, n, _) <- sources] ++ [(file, root) | Just (_, _, file, _) <- [extra]]) (maybe [] seedImports seed ++ [(n, [name | (name, _, _, _) <- importsOf n]) | n <- moduleNames]) (maybe (-1) seedFloor seed) (partials final) (kindsSaid final) (reverse (kindedTypes final)) (families final)
    ((linked, (bindings, builtins)), final) =
      runState
        (runReaderT run (Env Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty "" Nothing "" spelled Set.empty Map.empty Map.empty Map.empty Map.empty languages False Map.empty mempty))
        initial
    initial = case seed of
      Nothing -> St 0 IntMap.empty IntMap.empty IntMap.empty IntMap.empty [] IntMap.empty [] IntMap.empty [] [] IntMap.empty [] IntMap.empty [] [] [] [] IntMap.empty IntMap.empty [] IntMap.empty [] IntMap.empty IntMap.empty Map.empty IntMap.empty []
      Just s ->
        St
          (seedFloor s)
          -- The arrow is no package's own, and so is in none's names: an
          -- instance for functions is named by it in every package.
          (IntMap.insert (-1) arrow (seedNames s))
          (seedOrigins s)
          (seedSignatures s)
          (seedSynonyms s)
          (reverse (seedDataTypes s))
          (seedClasses s)
          (reverse (seedInstances s))
          (linkFields before)
          []
          []
          (linkFixities before)
          (seedNewtypes s)
          (linkStrict before)
          []
          []
          []
          []
          IntMap.empty
          IntMap.empty
          []
          (seedKinds s)
          []
          IntMap.empty
          (seedFamilies s)
          Map.empty
          (linkDefaults before)
          []
    -- What the packages before this one let be known.
    before = maybe mempty seedLinkage seed
    -- In a package every name is spelled with its module's: no module is
    -- the one a program starts in until packages are put together.
    spelled = maybe root (const "") seed
    moduleNames = nub [n | (_, n, _) <- sources]
    -- The module a prompt's expression is in, and whose names are spelled
    -- plainly in what is compiled: the last.
    root = case reverse moduleNames of
      n : _ -> n
      [] -> "Main"
    hasPrelude = "Prelude" `elem` moduleNames || Map.member "Prelude" (linkExports before)
    sourcesOf n = [(file, m) | (file, n', m) <- sources, n' == n]
    importsOf n =
      -- An import of the module itself is an error, and brings in nothing.
      [prelude file (name, qualified, alias, items) | (file, m) <- sourcesOf n, Decl.Import name qualified alias items <- moduleDecls m, name /= n]
        ++ [prelude file ("Prelude", False, Nothing, Nothing) | hasPrelude, n `notElem` ["Prelude", reportPrelude], null [() | (_, m) <- sourcesOf n, Decl.Import "Prelude" _ _ _ <- moduleDecls m], (file, _) <- take 1 (sourcesOf n)]
    -- The prelude of a module written with ReportPrelude, as the Haskell2010
    -- edition is, is another module, which lets out the Report's names
    -- only. It is still the Prelude to the module that imports it.
    prelude file (name, qualified, alias, items)
      | name == "Prelude", maybe False (Set.member ReportPrelude) (Map.lookup file languages), reportPrelude `elem` moduleNames || Map.member reportPrelude (linkExports before) = (reportPrelude, qualified, Just (fromMaybe "Prelude" alias), items)
      | otherwise = (name, qualified, alias, items)

    run = do
      (wiredTypes, wiredConstructors, wiredBuiltins) <- maybe wiredIn pure (linkWired before)
      -- Every name a module declares at its top, before what any of them
      -- is: types and their constructors, classes, and values.
      declaredTypes <- forM sources $ \(file, n, m) -> inPart n file . fmap concat . forM (moduleDecls m ++ concat [ds | Decl.Class _ ds <- moduleDecls m]) $ \d -> within d $ case d of
        Decl.TypeFamily t _ _ -> (\name -> [(t, name, [], [])]) <$> newName t TypeConstructor
        Decl.Data t _ cs -> do
          name <- newName t TypeConstructor
          ids <- forM cs (\(Syntax.Constructor c _) -> (,) c . NameId <$> fresh)
          pure [(t, name, ids, fieldsOf d)]
        Decl.TypeSynonym t _ _ -> (\name -> [(t, name, [], [])]) <$> newName t TypeSynonym
        _ -> pure []
      declaredClasses <- forM sources $ \(file, n, m) -> inPart n file . fmap concat . forM (moduleDecls m) $ \d -> within d $ case d of
        Decl.Class (classHead -> Just (_, c, _)) ds -> do
          name <- newName c TypeClass
          pure [(c, name, [x | Decl.Signature xs _ <- ds, x <- xs])]
        _ -> pure []
      -- A pattern synonym is a name among the constructors, and beside it
      -- two functions no program can name: one that matches, and if the
      -- synonym is an expression too, one that builds.
      declaredPatterns <- forM sources $ \(file, n, m) -> inPart n file . fmap concat . forM (moduleDecls m) $ \d -> within d $ case d of
        Decl.PatternSynonym p _ both _ -> (\name -> [(p, name, both || not (null (Decl.patternBuilder d)))]) <$> newName p Global
        _ -> pure []
      let patternsOf n = concat [ps | ((_, n', _), ps) <- zip sources declaredPatterns, n' == n]
      let parts = zip3 sources declaredTypes declaredClasses
          partsOf n = [(file, m, ts, cs) | ((file, n', m), ts, cs) <- parts, n' == n]
          methodsOf n = Map.fromList [(x, c) | (_, _, _, cs) <- partsOf n, (_, c, xs) <- cs, x <- xs]
          definedBy n =
            nub
              ( Map.keys (methodsOf n)
                  ++ concat [concatMap defines (grouped (moduleDecls m)) | (_, m) <- sourcesOf n]
                  ++ concat [fieldsOf d | (_, m) <- sourcesOf n, d <- moduleDecls m]
                  ++ concat [matcherName p : [builderName p | both] | (p, _, both) <- patternsOf n]
                  ++ [x | (_, m) <- sourcesOf n, Decl.Foreign _ x _ <- moduleDecls m]
                  ++ [exportName entity x | (_, m) <- sourcesOf n, Decl.ForeignExport entity x _ <- moduleDecls m]
                  ++ concat [name : maybe [] pure printer | n == root, Just (name, printer, _, _) <- [extra]]
              )
      globals <- fmap Map.fromList . forM (nub (root : moduleNames)) $ \n ->
        inPart n "" ((,) n . Map.fromList <$> mapM (\x -> (,) x <$> newName x (maybe Global Method (Map.lookup x (methodsOf n)))) (definedBy n))
      let globalsOf n = Map.findWithDefault Map.empty n globals
          -- A later declaration of a type, a class or a constructor
          -- replaces an earlier one of the same name in its module.
          own n =
            Scope
              { scopeValues = Map.map pure (globalsOf n),
                scopeConstructors = Map.fromList ([(c, [x]) | (_, _, ts, _) <- partsOf n, (_, _, cs, _) <- ts, (c, x) <- cs] ++ [(p, [x]) | (p, x, _) <- patternsOf n]),
                scopeTypes = Map.fromList [(t, [x]) | (_, _, ts, _) <- partsOf n, (t, x, _, _) <- ts],
                scopeClasses = Map.fromList [(c, [x]) | (_, _, _, cs) <- partsOf n, (c, x, _) <- cs]
              }
          -- What belongs to each type and class: constructors, or methods.
          belongings =
            IntMap.union
              ( IntMap.fromList
                  ( [(nameKey t, (cs, [(x, globalsOf n Map.! x) | x <- xs])) | ((_, n, _), ts, _) <- parts, (_, t, cs, xs) <- ts]
                      ++ [(nameKey c, ([], [(x, globalsOf n Map.! x) | x <- xs])) | ((_, n, _), _, cs) <- parts, (_, c, xs) <- cs]
                  )
              )
              (linkBelongings before)
          headerOf n = listToMaybe [h | (_, m) <- sourcesOf n, Just (_, Just h) <- [moduleHeader m]]
          imported known (name, qualified, alias, items) =
            let chosen = selected belongings items (Map.findWithDefault mempty name known)
             in (if qualified then mempty else chosen) <> qualifiedBy (fromMaybe name alias) chosen
          scopeIn known n
            | n `elem` lenient = shadowing (own n <> qualifiedBy n (own n)) (foldMap (imported known) (importsOf n))
            | otherwise = own n <> qualifiedBy n (own n) <> foldMap (imported known) (importsOf n)
          exportsIn known n = case headerOf n of
            Nothing -> own n
            Just items -> foldMap (exported n) items
              where
                scope = scopeIn known n
                exported _ (Syntax.Reexport m)
                  | m == n = own n
                  | otherwise = mconcat [selected belongings items' (Map.findWithDefault mempty name known) | (name, False, alias, items') <- importsOf n, fromMaybe name alias == m]
                exported _ item = unqualifiedNames (picked belongings scope item)
          settle known (rounds :: Int) =
            let next = Map.union (Map.fromList [(n, exportsIn known n) | n <- moduleNames]) (linkExports before)
             in if next == known || rounds == 0 then known else settle next (rounds - 1)
          exports = settle (Map.union (Map.fromList [(n, mempty) | n <- moduleNames]) (linkExports before)) (4 * length moduleNames + 8)
          wired = Scope Map.empty (Map.fromList [(c, [x]) | (c, x) <- wiredConstructors]) (Map.fromList [(t, [x]) | (t, x) <- wiredTypes]) Map.empty
          -- A module's own type of a name the language has hides that one.
          -- What the language has is the prelude's, to name it by.
          builtin' = wired <> (if hasPrelude then qualifiedBy "Prelude" wired else mempty)
          scopes = Map.fromList [(n, shadowing (scopeIn exports n) builtin') | n <- nub (root : moduleNames)]
          scopeOf n = Map.findWithDefault wired n scopes
          one = Map.mapMaybe (\case [x] -> Just x; _ -> Nothing)
          -- What the desugaring refers to is the prelude's, if there is one.
          -- It is all of what the prelude is made from, which is more than
          -- the prelude lets out.
          basis n
            | "Powder.Base" `elem` moduleNames = own "Powder.Base" <> wired
            | Just base <- linkBase before = base <> wired
            | hasPrelude && n /= "Prelude" = Map.findWithDefault mempty "Prelude" exports
            | otherwise = scopeOf n
          inModule n file =
            local $ \env ->
              env
                { envValues = one (scopeValues (scopeOf n)),
                  envTop = one (scopeValues (scopeOf n)),
                  envTopAll = scopeValues (scopeOf n),
                  envGlobals = one (scopeValues (basis n)),
                  envGlobalClasses = one (scopeClasses (basis n)),
                  envGenerics = Map.findWithDefault mempty "GHC.Generics" exports,
                  envConstructors = one (scopeConstructors (scopeOf n)),
                  envTypes = one (scopeTypes (scopeOf n)),
                  envClasses = one (scopeClasses (scopeOf n)),
                  envAmbiguous = Set.fromList (concat [Map.keys (Map.filter ((> 1) . length) (f (scopeOf n))) | f <- [scopeValues, scopeConstructors, scopeTypes, scopeClasses]]),
                  envModule = n,
                  envFile = file,
                  envOwnValues = globalsOf n,
                  envOwnConstructors = one (scopeConstructors (own n))
                }
      -- What is asked for must be there.
      forM_ sources $ \(file, n, m) -> inModule n file $ do
        forM_ (moduleDecls m) $ \d -> within d $ case d of
          Decl.Import name _ _ items
            | name `notElem` moduleNames && Map.notMember name (linkExports before) -> complain ("there is no module " ++ name)
            -- Said once, where what a file may not say is checked.
            | name == n -> pure ()
            | Just (False, wanted) <- items -> forM_ wanted $ \item -> do
                let theirs = Map.findWithDefault mempty name exports
                when (picked belongings theirs item == mempty) (complain (name ++ " does not export " ++ itemName item))
                forM_ (lacking belongings theirs item) (\x -> complain (name ++ " does not export " ++ x ++ " with " ++ itemName item))
          _ -> pure ()
        forM_ (moduleHeader m) $ \(_, listed) -> within m $ do
          forM_ (fromMaybe [] listed) $ \item -> case item of
            Syntax.Reexport other ->
              unless (other == n || other `elem` [fromMaybe name alias | (name, _, alias, _) <- importsOf n]) $
                complain ("the module " ++ other ++ " is exported, but is not imported")
            _ -> do
              when (picked belongings (scopeOf n) item == mempty) (complain (itemName item ++ " is exported, but is not in scope"))
              forM_ (lacking belongings (scopeOf n) item) (\x -> complain (x ++ " is exported with " ++ itemName item ++ ", which it does not belong to"))
          -- One name is one thing to whoever imports it.
          let mine = Map.findWithDefault mempty n exports
              twice = concat [Map.keys (Map.filter ((> 1) . length) (f mine)) | f <- [scopeValues, scopeConstructors, scopeTypes, scopeClasses]]
          forM_ (nub twice) (\x -> complain (x ++ " is exported as two different things"))
      -- The fixities each module declares, before any expression is read.
      forM_ sources $ \(file, n, m) -> inModule n file $ do
        env <- ask
        -- A fixity is for the value of the name and for the type of the
        -- name, where an operator is both.
        let find o =
              maybeToList (Map.lookup o (envOwnValues env) <|> Map.lookup o (envOwnConstructors env) <|> Map.lookup o (envValues env) <|> Map.lookup o (envConstructors env))
                ++ maybeToList (Map.lookup o (envTypes env))
                ++ maybeToList (Map.lookup o (envClasses env))
        declareFixities find (moduleDecls m ++ concat [ds | Decl.Class _ ds <- moduleDecls m])
      -- Types first, since everything else may mention them; then classes,
      -- before the instances that need to know their methods.
      forM_ parts $ \((file, n, m), ts, _) ->
        inModule n file (mapM_ (typeDecl (Map.fromList [(t, (x, map snd cs)) | (t, x, cs, _) <- ts])) (moduleDecls m))
      -- The functions from types to a type, and then their equations,
      -- which may be anywhere: beside them, in a class, in an instance.
      forM_ sources $ \(file, n, m) ->
        inModule n file (mapM_ familyDecl (moduleDecls m ++ concat [ds | Decl.Class _ ds <- moduleDecls m]))
      forM_ sources $ \(file, n, m) ->
        inModule n file (mapM_ familyEquation (moduleDecls m ++ concat [ds | Decl.Instance _ ds <- moduleDecls m]))
      forM_ parts $ \((file, n, m), _, cs) ->
        inModule n file (mapM_ (classDecl (Map.fromList [(c, x) | (c, x, _) <- cs])) (moduleDecls m))
      -- What a file may not say twice, or out of place; and what may not
      -- be defined by way of itself.
      forM_ parts $ \((file, n, m), ts, _) -> inModule n file . within m $ do
        let made = [c | (_, _, cs, _) <- ts, (c, _) <- cs]
            types' = [t | (t, _, _, _) <- ts]
            decls = moduleDecls m
            isImport = \case Decl.Import {} -> True; _ -> False
        forM_ (nub (made \\ nub made)) (\c -> complain ("the constructor " ++ c ++ " is declared more than once"))
        forM_ (nub (types' \\ nub types')) (\t -> complain ("the type " ++ t ++ " is declared more than once"))
        when (length [() | Decl.Default _ <- decls] > 1) (complain "a module has at most one default declaration")
        when (any isImport (dropWhile isImport decls)) (complain "an import comes after a declaration: imports go first")
        -- Modules may import each other, but what a module has it has
        -- already: importing itself could only bring in names from nowhere.
        forM_ decls $ \d -> case d of
          Decl.Import name _ _ _ | name == n -> within d (complain ("the module " ++ n ++ " imports itself"))
          _ -> pure ()
      known <- get
      let reaches next = go []
            where
              go seen = \case
                [] -> seen
                x : rest
                  | x `elem` seen -> go seen rest
                  | otherwise -> go (x : seen) (next x ++ rest)
          supers c = maybe [] (map (nameKey . constraintClass) . classSupers) (IntMap.lookup c (classes known))
          inSynonym c = maybe [] (typesIn . synonymType) (IntMap.lookup c (synonyms known))
          typesIn ty = case Type.typeForm ty of
            Type.Con c -> [nameKey c]
            Type.App f a -> typesIn f ++ typesIn a
            Type.Fun a b -> typesIn a ++ typesIn b
            _ -> []
          spelt c = maybe "?" nameText (IntMap.lookup c (names known))
      forM_ (IntMap.keys (classes known)) $ \c ->
        when (c `elem` reaches supers (supers c)) (complain ("the class " ++ spelt c ++ " is a superclass of itself"))
      forM_ (IntMap.keys (synonyms known)) $ \c ->
        when (c `elem` reaches inSynonym (inSynonym c)) (complain ("the type synonym " ++ spelt c ++ " is defined by way of itself"))
      forM_ parts $ \((file, n, m), ts, _) -> inModule n file . forM_ (moduleDecls m) $ \d -> do
        instanceDecl d
        case d of
          Decl.Default named -> within d (mapM type' named) >>= \found -> modify (\st -> st {defaults = (file, found) : defaults st})
          _ -> pure ()
        deriving' (Map.fromList [(t, x) | (t, x, _, _) <- ts]) (moduleDecls m) d
      resolved <- forM sources $ \(file, n, m) -> inModule n file $ do
        let gs = grouped (moduleDecls m)
        conflicts (concatMap defines gs) gs
        forM_ gs $ \g -> forM_ (filter (`Map.member` methodsOf n) (defines g)) $ \x ->
          within (declOf g) (complain (x ++ " is a method of a class, so it cannot be defined here"))
        declaredTypesOf [] (concatMap defines gs) (moduleDecls m)
        -- A C function is a definition with nothing to resolve in it.
        foreigns <- fmap concat . forM (moduleDecls m) $ \d -> case d of
          Decl.Foreign entity x ty -> within d $ do
            declared' <- signatureOf [] ty
            modify (\st -> st {signatures = IntMap.insert (nameKey (globalsOf n Map.! x)) declared' (signatures st)})
            -- What is said of it: that it is called by an address it is
            -- given (dynamic), that it makes a C function of one here
            -- (wrapper), or what C calls it, perhaps after a header, and
            -- with an & if it is the address that is wanted and not a
            -- call. Failing any of that it is called what it is here.
            let said = words (concatMap (\c -> if c == '&' then " & " else [c]) (fromMaybe "" entity))
                -- A header says where C declares the function, which
                -- nothing here reads.
                names = [w | w <- said, w `notElem` ["static", "&"], not (".h" `isSuffixOf` w)]
                symbol
                  | said `elem` [["dynamic"], ["wrapper"]] = concat said
                  | "&" `elem` said = '&' : fromMaybe x (listToMaybe (reverse names))
                  | otherwise = fromMaybe x (listToMaybe (reverse names))
            b <- node
            body <- mk (Foreign symbol) >>= plain
            pure . Binding b . Function (globalsOf n Map.! x) . pure . (\c -> Clause c [] body) <$> node
          -- A function made C's is a definition of its own, of the type C
          -- sees, that is the function: so that it is checked to have
          -- that type, and there is something to make C's.
          Decl.ForeignExport entity x ty -> within d $ do
            let hidden = globalsOf n Map.! exportName entity x
            declared' <- signatureOf [] ty
            modify (\st -> st {signatures = IntMap.insert (nameKey hidden) declared' (signatures st)})
            b <- node
            body <- variable x >>= mk >>= plain
            pure . Binding b . Function hidden . pure . (\c -> Clause c [] body) <$> node
          -- A pattern synonym's two functions. The one that matches gives
          -- a list: of what the synonym's variables are, in a tuple, if
          -- what it is given matches the synonym's pattern, and of nothing
          -- if not. A use of the synonym in a pattern is then a view
          -- pattern, of that function.
          Decl.PatternSynonym p parameters both definition -> within d $ do
            (ps, scope, vs) <- patternsViewed [definition]
            unless (nub parameters == parameters && all (`elem` map fst scope) parameters && all ((`elem` parameters) . fst) scope) $
              complain ("the pattern " ++ p ++ " stands for must bind the variables " ++ p ++ " is applied to, and no others")
            found <- extend scope (mapM (variable >=> mk) parameters) >>= tupleOf
            matched <- sequence [pure found, construct "[]"] >>= \arguments -> construct ":" >>= (`apply` arguments)
            nothing <- construct "[]"
            anything <- mkPat Pat.Wild
            b <- node
            hit <- (\c r -> Clause c ps (viewed vs r)) <$> node <*> plain matched
            miss <- (\c -> Clause c [anything]) <$> node <*> plain nothing
            -- The one that builds is the pattern read as an expression,
            -- which it can be if it is constructors, numbers and the
            -- variables.
            builder <- case Decl.patternBuilder d of
              -- Or it is what the synonym's own equations say.
              written'@(_ : _) -> do
                clauses <- fmap concat . forM written' $ \(left, r) -> within left $ case left of
                  SPat.Con p' arguments | p' == p -> do
                    c <- node
                    (ps', scope', vs') <- patternsViewed arguments
                    pure . Clause c ps' . viewed vs' <$> extend scope' (rhs r)
                  _ -> [] <$ complain ("after the where of the pattern synonym " ++ p ++ " come equations for " ++ p ++ ", applied to its arguments")
                b' <- node
                pure [Binding b' (Function (globalsOf n Map.! builderName p) clauses)]
              []
                | not both -> pure []
                | otherwise -> do
                    (ps', scope', _) <- patternsViewed [definition]
                    body <- mapM (patternExpr (map snd scope')) (take 1 ps')
                    xs <- mapM (\x -> mkPat (maybe Pat.Error Pat.Var (lookup x scope'))) parameters
                    b' <- node
                    forM body $ \e -> (\c r -> Binding b' (Function (globalsOf n Map.! builderName p) [Clause c xs r])) <$> node <*> plain e
            pure (Binding b (Function (globalsOf n Map.! matcherName p) [hit, miss]) : builder)
          _ -> pure []
        (,) [globalsOf n Map.! f | Equations _ f _ <- gs] . (++ foreigns) <$> mapM binding gs
      forM_ (take 1 sources) (\(file, n, _) -> inModule n file literalInstances)
      asked <- fmap (fromMaybe []) . forM extra $ \(name, printer, file, e) -> inModule root file . within e $ do
        let define x body = do
              n <- node
              c <- node
              Binding n . Function (globalsOf root Map.! x) . pure . Clause c [] <$> plain body
        value <- expr e >>= define name
        printed <- forM printer $ \p -> mk (Var (globalsOf root Map.! name)) >>= call "print" . pure >>= define p
        pure (value : maybe [] pure printed)
      -- A function a later part of its module defines again is left out.
      let later = drop 1 (scanr (\(fs, _) seen -> fs ++ seen) [] resolved)
          kept = [b | ((_, bs), redefined) <- zip resolved later, b <- bs, not (overridden redefined b)]
          method cls name = do
            c <- Map.lookup cls (one (scopeClasses (basis root)))
            x <- Map.lookup name (one (scopeValues (basis root)))
            case IntMap.lookup (nameKey c) belongings of
              Just (_, ms) | x `elem` map snd ms -> Just (c, x)
              _ -> Nothing
      pure (Linkage exports (if "Powder.Base" `elem` moduleNames then Just (own "Powder.Base") else linkBase before) belongings (Just (wiredTypes, wiredConstructors, wiredBuiltins)) IntMap.empty IntMap.empty IntMap.empty IntMap.empty, (kept ++ asked, wiredBuiltins {builtinNumeric = method "Num" "fromInteger", builtinEquality = method "Eq" "==", builtinFractional = (\(c, x) k -> (c, x, k)) <$> method "Fractional" "fromRational" <*> Map.lookup ":%" (one (scopeConstructors (basis root)))}))

    -- What the definition of a function made C's is called: nothing a
    -- program can write, with what C is to call the function.
    exportName entity x = "export#" ++ fromMaybe x (listToMaybe (reverse (words (fromMaybe "" entity))))

    overridden redefined b = case bindingKind b of
      Function f _ -> f `elem` redefined
      Destructure _ _ -> False

    fieldsOf = \case
      Decl.Data _ _ cs -> nub (concatMap Syntax.constructorFields cs)
      _ -> []

    itemName = \case
      Syntax.Value x -> x
      Syntax.Thing c _ -> c
      Syntax.Reexport m -> "module " ++ m
      Syntax.Pattern p -> "pattern " ++ p

-- Modules -------------------------------------------------------------------

-- | Names and what each may refer to, in the four ways a name can be
-- read: as a variable, a constructor, a type or a class. One with more
-- than one thing to refer to is ambiguous.
data Scope = Scope
  { scopeValues :: Map.Map Name [NameId],
    scopeConstructors :: Map.Map Name [NameId],
    scopeTypes :: Map.Map Name [NameId],
    scopeClasses :: Map.Map Name [NameId]
  }
  deriving (Eq)

instance Semigroup Scope where
  Scope a b c d <> Scope a' b' c' d' = Scope (both a a') (both b b') (both c c') (both d d')
    where
      both = Map.unionWith (\xs ys -> nub (xs ++ ys))

instance Monoid Scope where
  mempty = Scope Map.empty Map.empty Map.empty Map.empty

overScope :: (Map.Map Name [NameId] -> Map.Map Name [NameId]) -> Scope -> Scope
overScope f (Scope a b c d) = Scope (f a) (f b) (f c) (f d)

-- | The first scope's names in place of the second's.
shadowing :: Scope -> Scope -> Scope
shadowing (Scope a b c d) (Scope a' b' c' d') = Scope (Map.union a a') (Map.union b b') (Map.union c c') (Map.union d d')

qualifiedBy :: Name -> Scope -> Scope
qualifiedBy q = overScope (Map.mapKeys ((q ++ ".") ++))

unqualifiedNames :: Scope -> Scope
unqualifiedNames = overScope (Map.mapKeysWith (\xs ys -> nub (xs ++ ys)) unqualified)

-- | A name without the modules it is qualified by.
unqualified :: Name -> Name
unqualified name = case break (== '.') name of
  (q@(c : _), '.' : rest@(_ : _)) | isUpper c && all (\x -> isAlphaNum x || x `elem` "_'") q -> unqualified rest
  _ -> name

-- | What an item of an import or an export list picks out of a scope: a
-- variable; or a type or a class, with as much of what belongs to it as
-- the item says.
picked :: IntMap.IntMap ([(Name, NameId)], [(Name, NameId)]) -> Scope -> Syntax.Listed -> Scope
picked belongings scope = \case
  Syntax.Value x -> mempty {scopeValues = at x (scopeValues scope)}
  Syntax.Reexport _ -> mempty
  -- A pattern synonym, with the functions that are what it does.
  Syntax.Pattern p ->
    mempty
      { scopeConstructors = at p (scopeConstructors scope),
        scopeValues = Map.filterWithKey (\k _ -> k `elem` [beside p matcherName, beside p builderName]) (scopeValues scope)
      }
  Syntax.Thing c subordinates ->
    let things = Map.findWithDefault [] c (scopeTypes scope) ++ Map.findWithDefault [] c (scopeClasses scope)
        wanted x = case subordinates of
          Syntax.None -> False
          Syntax.All -> True
          Syntax.Some xs -> x `elem` xs
        belonging = [b | t <- things, Just b <- [IntMap.lookup (nameKey t) belongings]]
     in mempty
          { scopeTypes = at c (scopeTypes scope),
            scopeClasses = at c (scopeClasses scope),
            scopeConstructors = Map.fromListWith (++) [(x, [n]) | (cs, _) <- belonging, (x, n) <- cs, wanted x, n `Set.member` constructorsThere],
            scopeValues = Map.fromListWith (++) [(x, [n]) | (_, ms) <- belonging, (x, n) <- ms, wanted x, n `Set.member` valuesThere]
          }
  where
    at x = Map.filterWithKey (\k _ -> k == x)
    -- What belongs to a type is picked only if it is there: a type may
    -- be exported without its constructors.
    constructorsThere = Set.fromList (concat (Map.elems (scopeConstructors scope)))
    valuesThere = Set.fromList (concat (Map.elems (scopeValues scope)))

-- | The names an item asks for of what belongs to a type or a class,
-- that are not to be had.
lacking :: IntMap.IntMap ([(Name, NameId)], [(Name, NameId)]) -> Scope -> Syntax.Listed -> [Name]
lacking belongings scope item = case item of
  Syntax.Thing _ (Syntax.Some xs) -> let got = picked belongings scope item in [x | x <- xs, Map.notMember x (scopeConstructors got), Map.notMember x (scopeValues got)]
  _ -> []

-- | What an import takes of what a module exports: all of it, the items
-- it lists, or all but the items it hides.
selected :: IntMap.IntMap ([(Name, NameId)], [(Name, NameId)]) -> Maybe (Bool, [Syntax.Listed]) -> Scope -> Scope
selected belongings items exports = case items of
  Nothing -> exports
  Just (False, listed) -> foldMap (picked belongings exports) listed
  Just (True, hidden) -> foldl without exports hidden
  where
    without scope item =
      let gone = picked belongings scope item
          less f = Map.differenceWith (\xs ys -> case filter (`notElem` ys) xs of [] -> Nothing; left -> Just left) (f scope) (f gone)
          -- Hiding a name hides the constructor of that name too.
          named = case item of
            Syntax.Thing c _ -> Map.delete c
            _ -> id
       in Scope (less scopeValues) (named (less scopeConstructors)) (less scopeTypes) (less scopeClasses)

tupleName :: Int -> Name
tupleName n = "(" ++ replicate (n - 1) ',' ++ ")"

-- | What the function that matches a pattern synonym is called, and the
-- one that builds it: names no program can write.
matcherName, builderName :: Name -> Name
matcherName p = "match#" ++ p
builderName p = "build#" ++ p

-- | A name made from another, qualified as that one is.
beside :: Name -> (Name -> Name) -> Name
beside name f = take (length name - length (unqualified name)) name ++ f (unqualified name)

-- Plumbing ------------------------------------------------------------------

-- | Resolve a piece of syntax: what is made, and what is found wrong, is
-- placed there.
within :: (Ast a) => a -> M b -> M b
within a = local (\env -> if envPinned env then env else env {envOrigin = Just (Origin (envFile env) (Red.range (syntax a)))})

fresh :: M Int
fresh = state (\st -> (nextId st, st {nextId = nextId st + 1}))

-- | A new node, standing for the syntax being resolved.
node :: M NodeId
node = do
  n <- fresh
  from <- asks envOrigin
  forM_ from (\o -> modify (\st -> st {origins = IntMap.insert n o (origins st)}))
  pure (NodeId n)

newName :: Name -> NameSort -> M NameId
newName text sort = do
  n <- fresh
  home <- asks envModule
  modify (\st -> st {names = IntMap.insert n (NameInfo text sort home) (names st)})
  pure (NameId n)

-- | Resolve part of a module, before what is in scope in it is known.
inPart :: Name -> FilePath -> M a -> M a
inPart n file = local (\env -> env {envModule = n, envFile = file})

-- | The name a definition is of: the one that is the module's own, or
-- nearer than that.
binder :: Name -> M NameId
binder x = do
  env <- ask
  case Map.lookup x (envValues env) <|> Map.lookup x (envOwnValues env) of
    Just n -> pure n
    Nothing -> error ("Powder.Resolve: no name was made for the definition of " ++ x)

-- | Note the fixities some declarations give, to the names a lookup
-- finds for them.
declareFixities :: (Name -> [NameId]) -> [Syntax.Decl] -> M ()
declareFixities find ds = do
  let given = [(o, d) | d@(Decl.Fixity _ _ os) <- ds, o <- os]
  forM_ (nub (map fst given \\ nub (map fst given))) $ \o ->
    forM_ (take 1 (reverse [d | (o', d) <- given, o' == o])) (\d -> within d (complain ("a fixity is given for " ++ o ++ " more than once")))
  forM_ ds $ \d -> case d of
    Decl.Fixity assoc prec os -> do
      when (prec < 0 || prec > 9) (within d (complain "a precedence is from 0 to 9"))
      forM_ os $ \o -> case find o of
        [] -> within d (complain ("a fixity is given for " ++ o ++ ", which is not defined here"))
        xs -> forM_ xs (\x -> modify (\st -> st {fixityTable = IntMap.insert (nameKey x) (assoc, prec) (fixityTable st)}))
    _ -> pure ()

-- | The fixity of an operator in scope: what was declared for the thing
-- it refers to, or failing that the tightest, grouping to the left.
fixityOf :: Name -> M (Assoc, Int)
fixityOf name = do
  let constructed = take 1 (unqualified name) == ":" || any isUpper (take 1 (unqualified name))
  found <- asks (Map.lookup name . (if constructed then envConstructors else envValues))
  gets (\st -> maybe (LeftAssoc, 9) (\x -> IntMap.findWithDefault (LeftAssoc, 9) (nameKey x) (fixityTable st)) found)

-- | Say a name is not in scope, or if it is in scope twice over, that.
missing :: String -> Name -> M ()
missing what name = do
  ambiguous <- asks (Set.member name . envAmbiguous)
  complain (if ambiguous then name ++ " is ambiguous: more than one of the modules imported here exports it" else what ++ name)

complain :: String -> M ()
complain message = do
  env <- ask
  let found = Problem "name error" message (envFile env) (originSpan <$> envOrigin env) False
  modify (\st -> st {problems = found : problems st})

mk :: ExprKind -> M Expr
mk k = (`Expr` k) <$> node

mkPat :: Pat.Kind -> M Pat
mkPat k = (`Pat.Pat` k) <$> node

apply :: Expr -> [Expr] -> M Expr
apply = foldM (\f a -> mk (App f a))

-- | A name the desugaring relies on, such as @negate@ for a minus sign.
-- It is the one at the top level, whatever is in scope locally.
builtin :: Name -> M Expr
builtin name = do
  found <- asks (Map.lookup name . envGlobals)
  case found of
    Just n -> mk (Var n)
    Nothing -> complain (name ++ " is not defined, and this needs it") >> mk Error

-- | What a variable refers to.
variable :: Name -> M ExprKind
variable name = do
  found <- asks (Map.lookup name . envValues)
  case found of
    Just n -> pure (Var n)
    Nothing -> case Map.lookup name prims of
      Just p -> pure (Prim p)
      Nothing -> Error <$ missing "variable not in scope: " name
  where
    prims = Map.fromList [(primName p, p) | p <- [minBound ..]]

constructor :: Name -> M (Maybe NameId)
constructor name = do
  found <- asks (Map.lookup name . envConstructors)
  when (isNothing found) (missing "unknown constructor " name)
  pure found

construct :: Name -> M Expr
construct name =
  constructor name >>= \case
    Nothing -> mk Error
    Just k -> do
      synonym <- isSynonym k
      if not synonym
        then constructing k
        else
          asks (Map.lookup (beside name builderName) . envValues) >>= \case
            Just b -> mk (Var b)
            Nothing -> complain (name ++ " is a pattern synonym that is a pattern only: it is written with <-") >> mk Error

-- | Whether what a constructor's name refers to is a pattern synonym.
isSynonym :: NameId -> M Bool
isSynonym k = gets $ \st -> case IntMap.lookup (nameKey k) (names st) of
  Just (NameInfo _ Global _) -> True
  _ -> False

-- | A pattern that is a constructor applied to patterns, given the
-- constructor as it is spelled and how many view qualifiers there were
-- before its patterns were read. If the constructor is a pattern synonym
-- this is a view pattern of the function that matches it, which comes
-- before whatever its own patterns asked for.
applied :: Name -> NameId -> Int -> [Pat] -> M Pat
applied name k before ps = do
  synonym <- isSynonym k
  if not synonym
    then mkPat (Pat.Con k ps)
    else
      asks (Map.lookup (beside name matcherName) . envValues) >>= \case
        Nothing -> complain ("the pattern synonym " ++ name ++ " is in scope without what it stands for") >> mkPat Pat.Error
        Just matcher -> do
          x <- newName "view" Local
          result <- sequence [mk (Var matcher), mk (Var x)] >>= \case
            [f, argument] -> apply f [argument]
            _ -> mk Error
          inside <- tuplePatOf ps
          rest <- mkPat Pat.Wild
          one <- constructor ":" >>= mkPat . maybe Pat.Error (`Pat.Con` [inside, rest])
          builder <- asks (Map.lookup (beside name builderName) . envValues)
          modify (\st -> st {views = take before (views st) ++ Match one result : drop before (views st), synonymUses = (x, (builder, ps)) : synonymUses st})
          mkPat (Pat.Var x)

-- | Some expressions as one: a tuple of them, unless there is just one.
tupleOf :: [Expr] -> M Expr
tupleOf = \case
  [x] -> pure x
  xs -> construct (if null xs then "()" else tupleName (length xs)) >>= (`apply` xs)

tuplePatOf :: [Pat] -> M Pat
tuplePatOf = \case
  [p] -> pure p
  ps -> constructor (if null ps then "()" else tupleName (length ps)) >>= mkPat . maybe Pat.Error (`Pat.Con` ps)

-- | A pattern as the expression that makes what it matches, if it is
-- constructors, literals, pattern synonyms that can be built, and the
-- variables given.
patternExpr :: [NameId] -> Pat -> M Expr
patternExpr variables p = case Pat.patKind p of
  Pat.Var x
    | x `elem` variables -> mk (Var x)
    | otherwise ->
        gets (lookup x . synonymUses) >>= \case
          Just (Just builder, ps) -> do
            f <- mk (Var builder)
            mapM (patternExpr variables) ps >>= apply f
          _ -> no
  Pat.Lit l -> mk (Lit l)
  Pat.Con k ps -> do
    f <- constructing k
    mapM (patternExpr variables) ps >>= apply f
  Pat.Ordered _ q -> patternExpr variables q
  _ -> no
  where
    no = complain "this pattern cannot be read as an expression: write the synonym with <-" >> mk Error

-- | A constructor, to be applied to its fields. One with strict fields is
-- a function that works those out before it makes anything: the Report's
-- @C $! x@ for each of them (4.2.1). A newtype's constructor is not there
-- to be strict.
constructing :: NameId -> M Expr
constructing = constructingWith pure

-- | The same, with something done to the constructor itself first: it
-- may be applied to types.
constructingWith :: (Expr -> M Expr) -> NameId -> M Expr
constructingWith with k = do
  musts <- gets (IntMap.findWithDefault [] (nameKey k) . strict)
  new <- gets (elem k . newtypes)
  if new || not (or musts)
    then mk (Con k) >>= with
    else do
      xs <- mapM (const (newName "x" Local)) musts
      ps <- mapM (mkPat . Pat.Var) xs
      whole <- mk (Con k) >>= with >>= \c -> mapM (mk . Var) xs >>= apply c
      body <- foldM (\rest (x, must) -> if must then mk (Var x) >>= \v -> call "seq" [v, rest] else pure rest) whole (reverse (zip xs musts))
      mk (Lam ps body)

-- Types ----------------------------------------------------------------------

-- | How many fields a constructor has.
arityOf :: NameId -> M Int
arityOf c = gets $ \st -> case IntMap.lookup (nameKey c) (names st) of
  -- Not counting the dictionaries one with a context keeps.
  Just (NameInfo _ (Constructor info _) _) -> conArity info - sum [length context | d <- dataTypes st, (c', (_, context)) <- dataExistentials d, c' == c]
  _ -> 0

-- | Where in a constructor each of some fields is, each with what is
-- given for it. A field the constructor has not got is reported.
placed :: NameId -> [(Name, a)] -> M [(Int, a)]
placed c given = do
  let named = map fst given
  forM_ (nub (named \\ nub named)) (\f -> complain ("the field " ++ f ++ " is given more than once"))
  placedOnce c given

placedOnce :: NameId -> [(Name, a)] -> M [(Int, a)]
placedOnce c given = fmap catMaybes . forM given $ \(f, a) -> do
  -- Of the fields that go by the name, the constructor's.
  candidates <- asks (Map.findWithDefault [] f . envTopAll)
  known <- gets fields
  disambiguates <- enabled DisambiguateRecordFields
  case [i | x <- candidates, Just i <- [lookup c (IntMap.findWithDefault [] (nameKey x) known)]] of
    i : _
      | length candidates > 1 && not disambiguates ->
          Nothing <$ complain (f ++ " is the name of more than one thing here: the constructor says which only if DisambiguateRecordFields is on")
      | otherwise -> pure (Just (i, a))
    [] -> Nothing <$ complain (f ++ " is not a field of this constructor")

-- | The names of a constructor's fields, by where they are in it, given
-- the constructor as it is spelled. It has none if it is not known, or
-- was not declared with fields.
fieldsOfConstructor :: M (Name -> [(Int, Name)])
fieldsOfConstructor = do
  constructors <- asks envConstructors
  known <- gets fields
  named <- gets names
  pure $ \c -> case Map.lookup c constructors of
    Nothing -> []
    Just k -> [(i, nameText info) | (f, places) <- IntMap.toList known, (k', i) <- places, k' == k, Just info <- [IntMap.lookup f named]]

-- | The type of functions, whose identity is fixed beforehand.
arrow :: NameInfo
arrow = NameInfo "->" TypeConstructor ""

-- | The types and constructors that are there before any are declared:
-- those of literals and of what syntax builds, as the names they go by,
-- with the ones the language itself refers to.
wiredIn :: M ([(Name, NameId)], [(Name, NameId)], Builtins)
wiredIn = do
  integer <- newName "Integer" TypeConstructor
  int <- newName "Int" TypeConstructor
  -- The arrow has an identity fixed beforehand, which the checker knows.
  modify (\st -> st {names = IntMap.insert (-1) arrow (names st)})
  argument <- newName "a" TypeVariable
  result <- newName "b" TypeVariable
  addDataType (DataType (NameId (-1)) [argument, result] [] [] [])
  double <- newName "Double" TypeConstructor
  float <- newName "Float" TypeConstructor
  char <- newName "Char" TypeConstructor
  io <- newName "IO" TypeConstructor
  a <- newName "a" TypeVariable
  mapM_ (\(t, ps) -> addDataType (DataType t ps [] [] [])) [(integer, []), (int, []), (double, []), (float, []), (char, []), (io, [a])]
  (bool, boolCs) <- dataType "Bool" [] [("False", []), ("True", [])]
  (unit, unitCs) <- dataType "()" [] [("()", [])]
  -- The list's constructors mention the list type itself.
  list <- newName "[]" TypeConstructor
  element <- newName "a" TypeVariable
  let typeVariable x = (`Type.Type` Type.Var x) <$> node
  listOfElement <- do
    f <- (`Type.Type` Type.Con list) <$> node
    x <- typeVariable element
    (`Type.Type` Type.App f x) <$> node
  head' <- typeVariable element
  listCs <- constructorsOf list [element] [("[]", []), (":", [head', listOfElement])]
  -- Up to fifteen, which is as far as the Report says they must go.
  tuples <- forM [2 .. 24] $ \n -> do
    parameters <- mapM (\i -> newName ("t" ++ show i) TypeVariable) [1 .. n]
    fields <- mapM typeVariable parameters
    dataType (tupleName n) parameters [(tupleName n, fields)]
  pure
    ( [("->", NameId (-1)), ("Integer", integer), ("Int", int), ("Double", double), ("Float", float), ("Char", char), ("IO", io), ("Bool", bool), ("()", unit), ("[]", list)] ++ [(tupleName n, t) | (n, (t, _)) <- zip [2 ..] tuples],
      boolCs ++ unitCs ++ listCs ++ concatMap snd tuples,
      Builtins integer char io bool list unit int Nothing Nothing double float Nothing
    )

addDataType :: DataType -> M ()
addDataType d = modify (\st -> st {dataTypes = d : dataTypes st})

-- | Declare a data type and its constructors.
dataType :: Name -> [NameId] -> [(Name, [Type.Type])] -> M (NameId, [(Name, NameId)])
dataType t parameters cs = do
  name <- newName t TypeConstructor
  (,) name <$> constructorsOf name parameters cs

-- | Declare the constructors of a type that has its name, giving each
-- its own.
constructorsOf :: NameId -> [NameId] -> [(Name, [Type.Type])] -> M [(Name, NameId)]
constructorsOf t parameters cs = do
  ids <- mapM (const (NameId <$> fresh)) cs
  zip (map fst cs) ids <$ constructorsAs ids t parameters cs [] []

-- | Say what the constructors of a type are, under names they have been
-- given already. In what is compiled a constructor goes by its name, with
-- its module's before it unless that is the root.
constructorsAs :: [NameId] -> NameId -> [NameId] -> [(Name, [Type.Type])] -> [(NameId, ([NameId], [Constraint]))] -> [(NameId, [Type.Type])] -> M ()
constructorsAs ids t parameters cs existentials indices = do
  home <- asks envModule
  plainly <- asks ((|| null home) . (== home) . envRoot)
  forM_ (zip3 [0 ..] cs ids) $ \(tag, (c, fields), NameId n) -> do
    let spelling = if plainly then c else home ++ "." ++ c
        -- One with a context keeps a dictionary for each part of it,
        -- before its fields.
        kept = maybe 0 (length . snd) (lookup (NameId n) existentials)
    modify (\st -> st {names = IntMap.insert n (NameInfo c (Constructor (ConInfo spelling tag (kept + length fields) (length cs)) ids) home) (names st)})
  addDataType (DataType t parameters (zip ids (map snd cs)) existentials indices)
  -- Every type a module declares is one that can be asked for when a
  -- program runs: by its name, and its module's.
  typeable <- asks (Map.lookup "Typeable" . envGlobalClasses)
  called <- gets (fmap nameText . IntMap.lookup (nameKey t) . names)
  made <- asks (Map.member "mkTyCon" . envGlobals)
  forM_ ((,) <$> typeable <*> called) $ \(k, name) -> when (made && not (null home)) $ do
    anything <- mkPat Pat.Wild
    body <- mapM (mk . Lit . Text) [home, name] >>= call "mkTyCon"
    b <- equations "typeRepOf" [([anything], body)]
    i <- node
    at' <- (`Type.Type` Type.Con t) <$> node
    modify (\st -> st {instances = Instance i k [at'] [] [] [b] Exactly : instances st})

-- | The type variables in scope in the type of a constructor that is
-- given by its type: the ones its result applies the type to, each
-- standing for the parameter in its place. The result has to be the type
-- applied to variables, all different, since anything else is a GADT.
typedScope :: Name -> [(Name, NameId)] -> Syntax.Type -> M [(Name, NameId)]
typedScope t scope result = case typedShape result of
  Just (t', vs) | t' == t, length vs == length scope, nub vs == vs -> pure (zip vs (map snd scope))
  _ -> scope <$ complain ("the type of a constructor of " ++ t ++ " must end in " ++ unwords (t : map fst scope) ++ ", with any variables for its parameters so long as they are different")

-- | A type that is a constructor applied to variables: the two.
typedShape :: Syntax.Type -> Maybe (Name, [Name])
typedShape = \case
  SType.Paren inner -> typedShape inner
  SType.Con c -> Just (c, [])
  SType.App (SType.Con c) arguments -> (,) c <$> mapM variable' arguments
  _ -> Nothing
  where
    variable' = \case
      SType.Var v -> Just v
      SType.Paren inner -> variable' inner
      _ -> Nothing

-- | Note a function from types to a type, with its equations if it is
-- declared with them all.
familyDecl :: Syntax.Decl -> M ()
familyDecl d = within d $ case d of
  Decl.TypeFamily t parameters equations ->
    asks (Map.lookup t . envTypes) >>= \case
      Nothing -> pure ()
      Just f -> do
        given <- mapM (uncurry familyEquationOf) (fromMaybe [] equations)
        forM_ given $ \(f', _) -> unless (f' == Just f) (complain ("an equation of " ++ t ++ " is of " ++ t ++ " applied to types"))
        modify (\st -> st {families = IntMap.insert (nameKey f) (Family (length parameters) (isJust equations) (map snd given)) (families st)})
  _ -> pure ()

-- | Note an equation of a function from types to a type that is given
-- apart from it.
familyEquation :: Syntax.Decl -> M ()
familyEquation d = within d $ case d of
  Decl.TypeInstance left right ->
    familyEquationOf left right >>= \case
      (Just f, equation) -> do
        known <- gets (IntMap.lookup (nameKey f) . families)
        case known of
          Just family | not (familyClosed family) -> modify (\st -> st {families = IntMap.insert (nameKey f) family {familyEquations = familyEquations family ++ [equation]} (families st)})
          Just _ -> complain "this is a function from types whose equations are all where it is declared"
          Nothing -> complain "an equation is given for what is not a function from types to a type"
      (Nothing, _) -> complain "an equation is of a function from types applied to types"
  _ -> pure ()

-- | An equation of a function from types: the function, if what is on
-- the left is one applied to types, and the equation, whose type
-- variables are its own.
familyEquationOf :: SType.Type -> SType.Type -> M (Maybe NameId, ([NameId], [Type.Type], Type.Type))
familyEquationOf left right = do
  let (named, arguments) = case left of
        SType.App (SType.Con f) as -> (Just f, as)
        SType.Con f -> (Just f, [])
        SType.Paren (SType.App (SType.Con f) as) -> (Just f, as)
        SType.Operators [a, b] [f] -> (Just f, [a, b])
        _ -> (Nothing, [])
  f <- maybe (pure Nothing) (\x -> asks (Map.lookup x . envTypes)) named
  scope <- forM (nub (concatMap typeVariables (arguments ++ [right]))) (\x -> (,) x <$> newName x TypeVariable)
  withTypeVariables scope $ do
    arguments' <- mapM type' arguments
    right' <- type' right
    pure (f, (map snd scope, arguments', right'))

-- | What a declaration of a type says it is, given the names this module's
-- types were given. Gives the constructors it declares.
typeDecl :: Map.Map Name (NameId, [NameId]) -> Syntax.Decl -> M ()
typeDecl own d = within d $ case d of
  Decl.Data t parameters cs -> do
    named' <- typeParameters parameters
    -- A kind said after them stands for more parameters, with no names.
    let more = \case
          Just (Syntax.Arrow k rest) -> k : more (Just rest)
          _ -> []
    unnamed' <- forM (more (Decl.resultKind d)) $ \k -> do
      v <- newName "_" TypeVariable
      (,) "" v <$ sayKind k v
    let scope = named' ++ unnamed'
        (name, ids) = own Map.! t
    gadts <- enabled GADTs
    full <- forM cs $ \c@(Syntax.Constructor cname tys) -> within c $ do
      let assertions = concatMap assertionsOf (Syntax.constructorContext c)
          named = Syntax.constructorExistentials c
          -- Whether one given by its type makes the type at variables,
          -- all different, as every constructor written the other way
          -- does: then they are the type's parameters, as it names them.
          plainly = case fmap typedShape (Syntax.constructorResult c) of
            Just (Just (t', vs)) -> t' == t && length vs == length scope && nub vs == vs
            Just Nothing -> False
            Nothing -> True
      case Syntax.constructorResult c of
        -- With GADTs it may make the type at any types. Every type
        -- variable it has is then its own.
        Just result | gadts && not plainly -> do
          unless (null (Syntax.constructorFields c)) (complain "a constructor that makes its type at types of its own choosing does not have its fields named")
          own' <- forM (nub (named ++ concatMap typeVariables (tys ++ assertions ++ [result]))) (\x -> (,) x <$> newName x TypeVariable)
          withTypeVariables own' $ do
            fields' <- mapM type' tys
            context <- concat <$> mapM constraints assertions
            made <- type' result
            let spine ty arguments = case Type.typeForm ty of
                  Type.App f a -> spine f (a : arguments)
                  form -> (form, arguments)
            at' <- case spine made [] of
              (Type.Con made', arguments) | made' == name, length arguments == length scope -> pure arguments
              _ -> [] <$ complain ("the type of a constructor of " ++ t ++ " must end in " ++ t ++ " applied to " ++ show (length scope) ++ " types")
            pure ((cname, fields'), (map snd own', context), Just at')
        _ -> do
          -- A constructor given by its type names the type's parameters
          -- as it likes.
          scope' <- maybe (pure scope) (typedScope t scope) (Syntax.constructorResult c)
          -- With ExistentialQuantification it may have type variables of
          -- its own: those a forall names, or where it is given by its
          -- type those its fields have that its result has not; and a
          -- context.
          existential <- (gadts ||) <$> enabled ExistentialQuantification
          let unnamed = if isJust (Syntax.constructorResult c) then nub (concatMap typeVariables (tys ++ assertions)) \\ (map fst scope' ++ named) else []
              -- Named with a forall where it is given by its type, a
              -- variable may be one of the type's parameters.
              ofItsOwn = filter (`notElem` map fst scope') (if isJust (Syntax.constructorResult c) then named else []) ++ (if isJust (Syntax.constructorResult c) then [] else named) ++ unnamed
          unless (existential || (null ofItsOwn && null assertions)) $
            complain "a constructor has a context, or type variables of its own, only if ExistentialQuantification is on"
          unless ((null ofItsOwn && null assertions) || null (Syntax.constructorFields c)) $
            complain "a constructor with a context, or type variables of its own, does not have its fields named"
          own' <- forM (nub ofItsOwn) (\x -> (,) x <$> newName x TypeVariable)
          withTypeVariables (scope' ++ own') $ do
            fields' <- mapM type' tys
            context <- if existential then concat <$> mapM constraints assertions else pure []
            pure ((cname, fields'), (map snd own', context), Nothing)
    let declared = [con | (con, _, _) <- full]
    constructorsAs
      ids
      name
      (map snd scope)
      declared
      [(c, extra) | (c, (_, extra@(vs, context), at')) <- zip ids full, isJust at' || not (null vs && null context)]
      [(c, at') | (c, (_, _, Just at')) <- zip ids full]
    -- With StrictData a field is strict unless it is marked lazy.
    strictly <- enabled StrictData
    modify (\st -> st {strict = IntMap.union (IntMap.fromList (zip (map nameKey ids) (map (map (fromMaybe strictly) . Syntax.constructorMarks) cs))) (strict st)})
    when (Decl.isNewtype d) $
      if map (length . snd) declared == [1]
        then modify (\st -> st {newtypes = ids ++ newtypes st})
        else complain "a newtype has one constructor, with one field"
    -- A field is a function from the type to what is in the field, for
    -- each constructor that has it.
    forM_ (nub (concatMap Syntax.constructorFields cs)) $ \f -> do
      field <- binder f
      let places = [(c, i, ty) | (c, constructor', (_, tys)) <- zip3 ids cs declared, (i, f', ty) <- zip3 [0 ..] (Syntax.constructorFields constructor') tys, f' == f]
      modify (\st -> st {fields = IntMap.insert (nameKey field) [(c, i) | (c, i, _) <- places] (fields st)})
      clauses <- forM places $ \(c, i, _) -> do
        arity <- arityOf c
        x <- newName "x" Local
        ps <- forM [0 .. arity - 1] (\k -> mkPat (if k == i then Pat.Var x else Pat.Wild))
        p <- mkPat (Pat.Con c ps)
        Clause <$> node <*> pure [p] <*> (mk (Var x) >>= plain)
      n <- node
      modify (\st -> st {generated = Binding n (Function field clauses) : generated st})
      forM_ (take 1 places) $ \(_, _, ty) -> do
        let mkType form = (`Type.Type` form) <$> node
        self <- mkType (Type.Con name) >>= \hd -> foldM (\g v -> mkType (Type.Var v) >>= mkType . Type.App g) hd (map snd scope)
        whole <- mkType (Type.Fun self ty)
        modify (\st -> st {signatures = IntMap.insert (nameKey field) (Signature (map snd scope) [] whole) (signatures st)})
  Decl.TypeSynonym t parameters ty -> do
    scope <- typeParameters parameters
    -- With ConstraintKinds what it stands for may be constraints: each
    -- of its parts a class, or another such name, applied to types.
    kinds <- enabled ConstraintKinds
    classes' <- asks envClasses
    types <- asks envTypes
    known <- gets constraintSynonyms
    let asks' = \case
          Just ([], c, _) -> Map.member c classes' || maybe False (\n -> IntMap.member (nameKey n) known) (Map.lookup c types)
          _ -> False
    if kinds && all (asks' . classHead) (assertionsOf ty) && not (null (assertionsOf ty))
      then do
        stands <- withTypeVariables scope (concat <$> mapM constraints (assertionsOf ty))
        modify (\st -> st {constraintSynonyms = IntMap.insert (nameKey (fst (own Map.! t))) (map snd scope, stands) (constraintSynonyms st)})
      else do
        body <- withTypeVariables scope (type' ty)
        modify (\st -> st {synonyms = IntMap.insert (nameKey (fst (own Map.! t))) (Synonym (map snd scope) body) (synonyms st)})
  -- The kind of a type or a class, said apart from it.
  Decl.KindSignature t k -> do
    types <- asks envTypes
    classes' <- asks envClasses
    case Map.lookup t types <|> Map.lookup t classes' of
      Just n -> sayKind k n
      Nothing -> complain ("a kind is given for " ++ t ++ ", which is not a type or a class in scope")
  _ -> pure ()
  where
    typeParameters parameters = do
      forM_ (nub (parameters \\ nub parameters)) (\x -> complain ("the type variable " ++ x ++ " is given twice"))
      scope <- forM (nub parameters) (\x -> (,) x <$> newName x TypeVariable)
      -- Those the head gives kinds have them.
      scope <$ forM_ (Decl.kinds d) (\(x, k) -> forM_ (lookup x scope) (sayKind k))

withTypeVariables :: [(Name, NameId)] -> M a -> M a
withTypeVariables scope = local (\env -> env {envTypeVariables = Map.fromList scope})

-- | Resolve a type whose variables are in scope.
type' :: SType.Type -> M Type.Type
type' t = within t $ case t of
  SType.Var x ->
    asks (Map.lookup x . envTypeVariables) >>= \case
      Just n -> mkType (Type.Var n)
      Nothing -> complain ("type variable not in scope: " ++ x) >> mkType Type.Error
  SType.Con c -> typeConstructor c
  -- A number or a string, as a type: one type for each, the same
  -- wherever it is written. The first time one is met an instance is
  -- made that says which it is.
  SType.Lit spelt -> do
    promoted <- enabled DataKinds
    unless promoted (complain "a number or a string is a type only if DataKinds is on")
    literal spelt >>= \case
      Just l | Just (shown, method) <- literalType l ->
        gets (Map.lookup shown . literalTypes) >>= \case
          Just n -> mkType (Type.Con n)
          Nothing -> do
            n <- newName shown TypeConstructor
            modify (\st -> st {literalTypes = Map.insert shown n (literalTypes st), literalsPending = (n, l, method) : literalsPending st})
            literalInstances
            mkType (Type.Con n)
      _ -> complain "only a number or a string is a type" >> mkType Type.Error
  -- A list of types, as a type: the list's constructors, as types.
  SType.PromotedList elements -> do
    promoted <- enabled DataKinds
    unless promoted (complain "a list of types is a type only if DataKinds is on")
    nil <- asks (Map.lookup "[]" . envConstructors) >>= maybe (mkType Type.Error) (mkType . Type.Con)
    cons <- asks (Map.lookup ":" . envConstructors)
    resolved <- mapM type' elements
    foldM (\rest element -> maybe (mkType Type.Error) (mkType . Type.Con) cons >>= (`applyType` [element, rest])) nil (reverse resolved)
  -- The arrow written before its two types is the arrow between them.
  SType.App (SType.Con "->") [a, b] -> (Type.Fun <$> type' a <*> type' b) >>= mkType
  SType.App f arguments -> do
    f' <- type' f
    mapM type' arguments >>= applyType f'
  SType.Fun a b -> (Type.Fun <$> type' a <*> type' b) >>= mkType
  SType.Tuple [] -> typeConstructor "()"
  SType.Tuple ts -> do
    c <- typeConstructor (tupleName (length ts))
    mapM type' ts >>= applyType c
  SType.List element -> do
    c <- typeConstructor "[]"
    type' element >>= applyType c . pure
  SType.Paren inner -> type' inner
  SType.Qualified _ inner -> complain "a context can only be at the front of a signature or of the head of a class or an instance" >> type' inner
  -- With RankNTypes a type inside another may be for all types its
  -- variables could be, which are its own; with perhaps a context.
  SType.Forall vs whole -> do
    ranked <- enabled RankNTypes
    unless ranked (complain "a forall is inside a type, and not at the front of a signature, only if RankNTypes is on")
    own <- forM (nub vs) (\x -> (,) x <$> newName x TypeVariable)
    let (assertions, inner) = case whole of
          SType.Qualified context body -> (assertionsOf context, body)
          _ -> ([], whole)
    local (\env -> env {envTypeVariables = Map.union (Map.fromList own) (envTypeVariables env)}) $ do
      context <- concat <$> mapM constraints assertions
      body <- type' inner
      mkType (Type.Forall (map snd own) [(c, ts) | Constraint c ts <- context] body)
  -- A type said to have a kind is the type, and the kind is checked.
  SType.Kinded inner k -> do
    inner' <- type' inner
    said <- kindSyntax k
    inner' <$ modify (\st -> st {kindedTypes = (inner', said) : kindedTypes st})
  -- Types with operators between them group as the operators' fixities
  -- say, each operator a type's name applied to the two.
  SType.Operators operands operators -> do
    resolved <- mapM type' operands
    let climb :: Int -> [Type.Type] -> [Name] -> M (Type.Type, [Type.Type], [Name])
        climb least qs os = case qs of
          left : rest -> loop least left rest os
          [] -> (,,) <$> mkType Type.Error <*> pure [] <*> pure os
        loop least left rest = \case
          o : os -> do
            (assoc, prec) <- typeFixity o
            if prec < least
              then pure (left, rest, o : os)
              else do
                (right, rest', os') <- climb (if assoc == RightAssoc then prec else prec + 1) rest os
                joined <- typeConstructor o >>= (`applyType` [left, right])
                loop least joined rest' os'
          [] -> pure (left, rest, [])
    (\(whole, _, _) -> whole) <$> climb 0 resolved operators
  -- A part left out is a variable of its own, which whoever reads the
  -- type takes note of.
  SType.Wild -> do
    allowed <- enabled PartialTypeSignatures
    unless allowed (complain "a part of a type is left out, as _, only if PartialTypeSignatures is on")
    blank <- newName "_" TypeVariable
    modify (\st -> st {wildcards = wildcards st ++ [blank]})
    mkType (Type.Var blank)
  where
    mkType form = (`Type.Type` form) <$> node
    applyType = foldM (\f a -> mkType (Type.App f a))
    -- How a number or a string is shown as a type, and the method of
    -- the class that says which one a type is.
    literalType = \case
      Number n | n >= 0 -> Just (show n, "natInteger")
      Text s -> Just (show s, "symbolString")
      _ -> Nothing
    -- With DataKinds a constructor is a type too: where a tick says so,
    -- or where no type goes by the name.
    typeConstructor c = do
      promoted <- enabled DataKinds
      let (ticked, plain) = case c of
            '\'' : rest -> (True, rest)
            _ -> (False, c)
      ofType <- asks (Map.lookup plain . envTypes)
      ofConstructor <- asks (Map.lookup plain . envConstructors)
      case (ofType, ofConstructor) of
        (Just n, _) | not ticked -> mkType (Type.Con n)
        (_, Just k) | promoted -> mkType (Type.Con k)
        (Just n, _) -> mkType (Type.Con n)
        _ -> missing "unknown type " c >> mkType Type.Error

-- | The type variables a type mentions, as they are spelled, in order.
typeVariables :: SType.Type -> [Name]
typeVariables = \case
  SType.Var x -> [x]
  SType.Con _ -> []
  SType.App f arguments -> concatMap typeVariables (f : arguments)
  SType.Fun a b -> typeVariables a ++ typeVariables b
  SType.Tuple ts -> concatMap typeVariables ts
  SType.List element -> typeVariables element
  SType.Paren inner -> typeVariables inner
  SType.Qualified context inner -> typeVariables context ++ typeVariables inner
  SType.Forall vs inner -> filter (`notElem` vs) (typeVariables inner)
  SType.Wild -> []
  SType.Kinded inner _ -> typeVariables inner
  SType.Operators operands _ -> concatMap typeVariables operands
  SType.PromotedList elements -> concatMap typeVariables elements
  SType.Lit _ -> []

-- | Note the types a scope's signatures declare, for the names that scope
-- defines, which are in scope. A signature quantifies over the variables
-- it mentions: first those it is given, which are a class's, and then its
-- own.
declaredTypesOf :: [(Name, NameId)] -> [Name] -> [Syntax.Decl] -> M ()
declaredTypesOf given defined ds = do
  let signed = [(x, d) | d@(Decl.Signature xs _) <- ds, x <- xs]
  forM_ (nub (map fst signed \\ nub (map fst signed))) $ \x ->
    forM_ (take 1 (reverse [d | (x', d) <- signed, x' == x])) (\d -> within d (complain ("there is more than one type signature for " ++ x)))
  declaredTypesIn given defined ds

declaredTypesIn :: [(Name, NameId)] -> [Name] -> [Syntax.Decl] -> M ()
declaredTypesIn given defined ds = forM_ ds $ \d -> within d $ case d of
  Decl.Signature xs ty -> do
    (declared, own, partial) <- signatureFull given ty
    scoping <- enabled ScopedTypeVariables
    forM_ xs $ \x ->
      if x `notElem` defined
        then complain ("the type signature for " ++ x ++ " has no definition to go with it")
        else do
          NameId n <- binder x
          -- A type with parts left out is not what the name is declared
          -- to have: its type is found, and has that shape.
          if partial
            then modify (\st -> st {partials = IntMap.insert n declared (partials st)})
            else modify (\st -> st {signatures = IntMap.insert n declared (signatures st)})
          -- Variables named with a forall are in scope in the definition.
          when (scoping && explicit ty && not partial) (modify (\st -> st {scopedVariables = IntMap.insert n own (scopedVariables st)}))
  _ -> pure ()
  where
    explicit = \case
      SType.Forall _ _ -> True
      _ -> False

-- | A type as a signature gives it, with its context if it has one, over
-- the variables it is given and then its own.
signatureOf :: [(Name, NameId)] -> SType.Type -> M Signature
signatureOf given ty = do
  (declared, _, partial) <- signatureFull given ty
  when partial (complain "a type with parts left out, as _, can only be the signature of a definition")
  pure declared

-- | The same, with its own variables as they are spelled, and whether
-- it has parts left out: an underscore, or with NamedWildCards a variable
-- that starts with one. Then every variable of it stands for a part left
-- out. A type variable that is in scope already is that one, and not the
-- signature's own: so it is where the variables of an enclosing
-- signature are in scope.
signatureFull :: [(Name, NameId)] -> SType.Type -> M (Signature, [(Name, NameId)], Bool)
signatureFull given whole = do
  outer <- asks (Map.toList . envTypeVariables)
  let (named, ty) = case whole of
        SType.Forall vs inner -> (Just vs, inner)
        _ -> (Nothing, whole)
      (assertions, body) = case ty of
        SType.Qualified context inner -> (assertionsOf context, inner)
        _ -> ([], ty)
      -- One it names itself is its own, whatever else goes by the name.
      around = [(x, n) | (x, n) <- outer, x `notElem` map fst given, x `notElem` fromMaybe [] named]
      mentioned = nub (typeVariables ty) \\ (map fst given ++ map fst around)
  forM_ named $ \vs -> forM_ (mentioned \\ vs) (\x -> complain ("the type variable " ++ x ++ " is not among those the forall names"))
  own <- forM (maybe mentioned (\vs -> nub (vs ++ mentioned)) named) (\x -> (,) x <$> newName x TypeVariable)
  forM_ (SType.forallKinds whole) (\(x, k) -> forM_ (lookup x own) (sayKind k))
  let scope = given ++ own
  modify (\st -> st {wildcards = []})
  (constrained, resolved) <- withTypeVariables (scope ++ around) ((,) . concat <$> mapM constraints assertions <*> type' body)
  blanks <- gets wildcards
  modify (\st -> st {wildcards = []})
  namedWild <- enabled NamedWildCards
  let partial = not (null blanks) || (namedWild && any ((== "_") . take 1 . fst) own)
  pure (Signature (map snd scope ++ blanks) constrained resolved, own, partial)

-- | For each number or string met as a type that has none yet, the
-- instance of the class that says which it is: once that class is known,
-- which it is not while the types of a module are first read.
literalInstances :: M ()
literalInstances = do
  pending <- gets literalsPending
  globals <- asks envGlobals
  known <- gets classes
  left <- fmap concat . forM pending $ \(n, l, method) ->
    case [k | k <- IntMap.elems known, Just m <- [Map.lookup method globals], m `elem` classMethods k] of
      k : _ -> do
        -- Its kind is the one the class says its variable is of.
        forM_ (take 1 (classVariables k)) $ \v ->
          gets (IntMap.lookup (nameKey v) . kindsSaid) >>= mapM_ (\said -> modify (\st -> st {kindsSaid = IntMap.insert (nameKey n) said (kindsSaid st)}))
        anything <- mkPat Pat.Wild
        b <- mk (Lit l) >>= \body -> equations method [([anything], body)]
        i <- node
        at' <- (`Type.Type` Type.Con n) <$> node
        [] <$ modify (\st -> st {instances = Instance i (className k) [at'] [] [] [b] Exactly : instances st})
      [] -> pure [(n, l, method)]
  modify (\st -> st {literalsPending = left})

-- | Note the kind something is said to have.
sayKind :: Syntax.Kind -> NameId -> M ()
sayKind k n = kindSyntax k >>= \said -> modify (\st -> st {kindsSaid = IntMap.insert (nameKey n) said (kindsSaid st)})

-- | A kind as it is written, with the data type it names found.
kindSyntax :: Syntax.Kind -> M KindSyntax
kindSyntax = \case
  Syntax.Star -> pure KindStar
  Syntax.Arrow a r -> KindArrow <$> kindSyntax a <*> kindSyntax r
  Syntax.ListOf k -> KindList <$> kindSyntax k
  Syntax.Variable v -> do
    poly <- enabled PolyKinds
    unless poly (complain ("a kind has a variable in it, " ++ v ++ ", only if PolyKinds is on"))
    pure (KindVariable v)
  -- A data type is a kind, of the types its constructors are.
  Syntax.Named c -> do
    promoted <- enabled DataKinds
    unless promoted (complain ("a data type, " ++ c ++ ", is a kind only if DataKinds is on"))
    asks (Map.lookup c . envTypes) >>= \case
      Just t -> pure (KindNamed t)
      Nothing -> KindStar <$ complain ("unknown kind " ++ c)

-- | The fixity of an operator that is a type's name: what was declared
-- for the type, or for the constructor of the same name, since one
-- declaration is for both; or failing that the tightest, to the left.
typeFixity :: Name -> M (Assoc, Int)
typeFixity name = do
  ofType <- asks (Map.lookup name . envTypes)
  ofClass <- asks (Map.lookup name . envClasses)
  ofConstructor <- asks (Map.lookup name . envConstructors)
  known <- gets fixityTable
  pure (fromMaybe (LeftAssoc, 9) (listToMaybe [f | Just x <- [ofType, ofClass, ofConstructor], Just f <- [IntMap.lookup (nameKey x) known]]))

-- | Do something with the type variables a name's signature brought into
-- scope, if it brought any.
scopedBy :: NameId -> M a -> M a
scopedBy name inner = do
  found <- gets (IntMap.findWithDefault [] (nameKey name) . scopedVariables)
  local (\env -> env {envTypeVariables = Map.union (Map.fromList found) (envTypeVariables env)}) inner

-- | Do something with some type variables in scope, if ScopedTypeVariables
-- is on: an instance's, in its methods, and a class's in its defaults.
scopedIf :: [(Name, NameId)] -> M a -> M a
scopedIf scope inner = do
  scoping <- enabled ScopedTypeVariables
  if scoping then local (\env -> env {envTypeVariables = Map.union (Map.fromList scope) (envTypeVariables env)}) inner else inner

-- Classes -------------------------------------------------------------------

-- | The head of a class or an instance taken apart: the assertions of its
-- context, the class, and what the class is applied to.
classHead :: SType.Type -> Maybe ([SType.Type], Name, [SType.Type])
classHead = \case
  SType.Qualified context inner -> (\(_, c, ts) -> (assertionsOf context, c, ts)) <$> classHead inner
  SType.App (SType.Con c) ts -> Just ([], c, ts)
  SType.Con c -> Just ([], c, [])
  -- A class whose name is an operator, between its two types.
  SType.Operators [left, right] [c] -> Just ([], c, [left, right])
  SType.Paren inner -> classHead inner
  -- Its variables may be named, which says nothing more.
  SType.Forall _ inner -> classHead inner
  _ -> Nothing

-- | The assertions of a context: one, or several in parentheses.
assertionsOf :: SType.Type -> [SType.Type]
assertionsOf = \case
  SType.Tuple ts -> ts
  SType.Paren t -> assertionsOf t
  t -> [t]

-- | Resolve one assertion of a context, whose type variables are in
-- scope: a class applied to as many types as it has variables. As the
-- Report has it that is one type, which is a variable or a variable
-- applied to types: any other needs FlexibleContexts, and more or fewer
-- than one MultiParamTypeClasses. With ConstraintKinds it may also be a
-- name some constraints were given, and so be several.
constraints :: SType.Type -> M [Constraint]
constraints t = within t $ case classHead t of
  Just ([], c, arguments) -> do
    found <- asks (Map.lookup c . envClasses)
    alias <- asks (Map.lookup c . envTypes) >>= \x -> gets (\st -> x >>= \n -> IntMap.lookup (nameKey n) (constraintSynonyms st))
    case (found, alias) of
      (Just k, _) -> do
        flexible <- enabled FlexibleContexts
        several <- enabled MultiParamTypeClasses
        let plain = \case
              SType.Var _ -> True
              SType.App (SType.Var _) _ -> True
              SType.Paren inner -> plain inner
              SType.Kinded inner _ -> plain inner
              _ -> False
        unless (flexible || all plain arguments) (complain "a class is asked of something other than a type variable, or one applied to types, only if FlexibleContexts is on")
        unless (several || length arguments == 1) (complain "a class is asked of one type, unless MultiParamTypeClasses is on")
        pure . Constraint k <$> mapM type' arguments
      (Nothing, Just (parameters, stands)) -> do
        given <- mapM type' arguments
        if length given /= length parameters
          then [] <$ complain (c ++ " stands for constraints on " ++ show (length parameters) ++ " types, and is given " ++ show (length given))
          else forM stands $ \(Constraint k us) -> Constraint k <$> mapM (\u -> foldM (\acc (v, by) -> substituted v by acc) u (zip parameters given)) us
      (Nothing, Nothing) -> [] <$ complain ("unknown class " ++ c)
  _ -> [] <$ complain "a context is made of classes, each applied to types"

-- | A class asked of one type variable.
constraintOn :: NameId -> NameId -> M Constraint
constraintOn c v = (\t -> Constraint c [t]) . (`Type.Type` Type.Var v) <$> node

-- | A type constructor applied to type variables.
appliedTo :: NameId -> [NameId] -> M Type.Type
appliedTo t vs = do
  hd <- (`Type.Type` Type.Con t) <$> node
  foldM applied' hd vs
  where
    applied' f v = do
      a <- (`Type.Type` Type.Var v) <$> node
      (`Type.Type` Type.App f a) <$> node

-- | What a class declaration says the class is, given the names this
-- module's classes were given. Its methods are in scope already.
classDecl :: Map.Map Name NameId -> Syntax.Decl -> M ()
classDecl own d = within d $ case d of
  Decl.Class (classHead -> Just (context, c, parameters)) ds | Just named <- mapM variableOf parameters -> do
    several <- enabled MultiParamTypeClasses
    unless (several || length named == 1) (complain "a class has one type variable, unless MultiParamTypeClasses is on")
    forM_ (nub (map fst named \\ nub (map fst named))) (\a -> complain ("the type variable " ++ a ++ " is given twice"))
    scope <- forM named (\(a, _) -> (,) a <$> newName a TypeVariable)
    -- A variable of the class may be given a kind.
    forM_ (zip named scope) (\((_, k), (_, v)) -> forM_ k (`sayKind` v))
    supers <- withTypeVariables scope (concat <$> mapM constraints context)
    -- What decides what: each a place among the class's variables.
    dependencies <- fmap catMaybes . forM (Decl.dependencies d) $ \(from, to) -> do
      let place a = elemIndex a (map fst named)
      case (mapM place from, mapM place to) of
        (Just is, Just js) -> pure (Just (is, js))
        _ -> Nothing <$ complain "a functional dependency is between the class's own type variables"
    let methods = [x | Decl.Signature xs _ <- ds, x <- xs]
        groups = grouped ds
        variables = map fst named
    -- A method's type is about the class's variables, and may not ask
    -- more of them than the class does.
    forM_ ds $ \m -> case m of
      Decl.Signature _ ty -> within m $ do
        let (asked, body) = case ty of
              SType.Qualified given inner -> (assertionsOf given, inner)
              _ -> ([], ty)
        -- With AllowAmbiguousTypes it need not: then only applying the
        -- method to a type says which instance is meant.
        ambiguous <- enabled AllowAmbiguousTypes
        unless (ambiguous || null variables || any (`elem` typeVariables body) variables) (complain ("the type of a method must mention the class's variable, " ++ unwords variables))
        more <- enabled ConstrainedClassMethods
        forM_ asked $ \case
          SType.App _ [SType.Var x] | x `elem` variables, not more -> complain ("a method's context asks more of the class's variable, " ++ x ++ ", only if ConstrainedClassMethods is on")
          _ -> pure ()
      _ -> pure ()
    methodIds <- mapM binder (nub methods)
    declaredTypesOf scope methods ds
    forM_ ds $ \m -> case m of
      Decl.DefaultSignature xs _ -> within m $ forM_ xs $ \x ->
        unless (x `elem` methods) (complain ("the default of " ++ x ++ " is given a type, and it is not a method of the class"))
      _ -> pure ()
    defaults <- fmap concat . forM groups $ \g -> case g of
      Equations _ f _ | f `elem` methods -> do
        b <- scopedIf scope (binding g)
        -- A default with a type of its own is a function of its own, of
        -- that type and for the class's instances: an instance that does
        -- not define the method has it be that function, and so must
        -- have what the function asks.
        case ([ty | Decl.DefaultSignature xs ty <- ds, f `elem` xs], b) of
          (ty : _, Binding n (Function m clauses)) -> do
            (Signature vs context t, _, _) <- signatureFull scope ty
            d <- newName ("default#" ++ f) Global
            arguments <- forM scope (\(_, v) -> (`Type.Type` Type.Var v) <$> node)
            modify $ \st ->
              st
                { signatures = IntMap.insert (nameKey d) (Signature vs (Constraint (own Map.! c) arguments : context) t) (signatures st),
                  generated = Binding n (Function d clauses) : generated st,
                  defaultSigned = IntMap.insert (nameKey m) d (defaultSigned st)
                }
            pure []
          _ -> pure [b]
      _ -> [] <$ within (declOf g) (complain "a class can only define its own methods")
    let cls = Class (own Map.! c) (map snd scope) supers dependencies methodIds defaults
    modify (\st -> st {classes = IntMap.insert (nameKey (className cls)) cls (classes st)})
  Decl.Class _ _ -> complain "a class is declared as its name and its type variables"
  _ -> pure ()
  where
    variableOf = \case
      SType.Var a -> Just (a, Nothing)
      SType.Kinded (SType.Var a) k -> Just (a, Just k)
      SType.Paren inner -> variableOf inner
      _ -> Nothing

-- | For the methods of a class that an instance does not define, and
-- whose defaults have types of their own: each as its default.
defaultedFor :: NameId -> [Binding] -> M [Binding]
defaultedFor k bs = do
  methods <- gets (maybe [] classMethods . IntMap.lookup (nameKey k) . classes)
  table <- gets defaultSigned
  forM [(m, d) | m <- methods, m `notElem` [m' | Binding _ (Function m' _) <- bs], Just d <- [IntMap.lookup (nameKey m) table]] $ \(m, d) -> do
    body <- mk (Var d) >>= plain
    n <- node
    (\c -> Binding n (Function m [Clause c [] body])) <$> node

-- | Note an instance. There may not be another of its class at the same
-- types, whatever each calls its variables.
addInstance :: Instance -> M ()
addInstance inst = do
  clash <- gets (any (\other -> instanceClass other == instanceClass inst && shape other == shape inst) . instances)
  when clash (complain "there is already an instance of this class for this type")
  modify (\st -> st {instances = inst : instances st})
  where
    -- An instance's types, with each variable as when it is first met.
    shape i = map (form (zip (instanceVariables i) [0 :: Int ..])) (instanceHead i)
    form numbered ty = case Type.typeForm ty of
      Type.Var v -> "$" ++ maybe "?" show (lookup v numbered)
      Type.Con c -> "#" ++ show (nameKey c)
      Type.App f a -> "(" ++ form numbered f ++ " " ++ form numbered a ++ ")"
      Type.Fun a b -> "(" ++ form numbered a ++ " -> " ++ form numbered b ++ ")"
      Type.Forall _ _ inner -> "(forall " ++ form numbered inner ++ ")"
      Type.Error -> "!"

-- | What an instance declaration says. The classes are all known.
instanceDecl :: Syntax.Decl -> M ()
instanceDecl d = within d $ case d of
  Decl.Instance (classHead -> Just (context, c, arguments)) ds ->
    asks (Map.lookup c . envClasses) >>= \case
      Nothing -> complain ("unknown class " ++ c)
      Just k -> do
        -- With InstanceSigs a method's type may be said again. It is
        -- read, and what the class says is what the method is checked
        -- against.
        signatures' <- enabled InstanceSigs
        forM_ ds $ \s -> case s of
          Decl.Signature _ _ | not signatures' -> within s (complain "an instance gives its methods type signatures only if InstanceSigs is on: the class gives them")
          _ -> pure ()
        scope <- forM (nub (concatMap typeVariables arguments)) (\x -> (,) x <$> newName x TypeVariable)
        -- One that names its variables with a forall brings them into
        -- scope in the method, as the variables the class's own
        -- signature has where they are.
        scoping <- enabled ScopedTypeVariables
        said <- fmap concat . forM ds $ \s -> case s of
          Decl.Signature xs ty | signatures' -> within s $ do
            (Signature _ _ written', own, _) <- withTypeVariables scope (signatureFull [] ty)
            pure [(x, (written', own)) | scoping, SType.Forall _ _ <- [ty], x <- xs]
          _ -> pure []
        declared' <- gets signatures
        (heads, context') <- withTypeVariables scope ((,) <$> mapM type' arguments <*> (concat <$> mapM constraints context))
        cls <- gets (IntMap.lookup (nameKey k) . classes)
        aliases <- gets synonyms
        flexible <- enabled FlexibleInstances
        synonymous <- (flexible ||) <$> enabled TypeSynonymInstances
        let methods = maybe [] classMethods cls
            arity = maybe 1 (length . classVariables) cls
            -- Where the two types have a variable each in one place.
            matched ofClass m w = case (Type.typeForm m, Type.typeForm w) of
              (Type.Var u, Type.Var v) | u `notElem` ofClass -> [(v, u)]
              (Type.App f x, Type.App g y) -> matched ofClass f g ++ matched ofClass x y
              (Type.Fun x y, Type.Fun x' y') -> matched ofClass x x' ++ matched ofClass y y'
              _ -> []
            scopeOfMethod f m = case (lookup f said, IntMap.lookup (nameKey m) declared') of
              (Just (written', own), Just (Signature vs _ theirs)) -> [(x, u) | (x, v) <- own, Just u <- [lookup v (matched (take arity vs) theirs written')]]
              _ -> []
            -- As the Report has an instance's type: a type constructor,
            -- not a synonym, applied to type variables, each different.
            plain ty = case typeSpine ty [] of
              (Type.Con tc, applied') ->
                let variables = [v | Type.Var v <- map Type.typeForm applied']
                 in (synonymous || IntMap.notMember (nameKey tc) aliases) && length variables == length applied' && nub variables == variables
              _ -> False
            wrong ty = case Type.typeForm ty of
              Type.Error -> True
              _ -> False
        when (isJust cls && length arguments /= arity) $
          complain ("the class " ++ c ++ " is of " ++ show arity ++ " types, and this instance is for " ++ show (length arguments))
        n <- node
        bs <- fmap concat . forM (grouped ds) $ \g -> case g of
          Equations _ f _ -> do
            -- A method is the class's, whatever else goes by its name.
            spelled <- gets (\st -> [m | m <- methods, fmap nameText (IntMap.lookup (nameKey m) (names st)) == Just (unqualified f)])
            let found = listToMaybe spelled
            -- It must be in scope, though under any name.
            visible <- asks (\env -> maybe False (`elem` Map.elems (envValues env)) found)
            if visible
              then pure <$> scopedIf (scope ++ maybe [] (scopeOfMethod (unqualified f)) found) (bindingAs found g)
              else [] <$ within (declOf g) (complain (f ++ " is not a method of the class " ++ c))
          Destructuring {} -> [] <$ within (declOf g) (complain "an instance can only define methods")
        let overlap = case Decl.overlap d of
              Just "OVERLAPPING" -> Overlapping
              Just "OVERLAPPABLE" -> Overlappable
              Just "OVERLAPS" -> Overlaps
              _ -> Exactly
        case () of
          _
            | any wrong heads -> pure ()
            | flexible || all plain heads -> defaultedFor k bs >>= \more -> addInstance (Instance n k heads (map snd scope) context' (bs ++ more) overlap)
            | otherwise -> complain "an instance is for a type constructor applied to type variables, each different, unless FlexibleInstances is on"
  Decl.Instance _ _ -> complain "an instance is declared as a class applied to types"
  _ -> pure ()
  where
    typeSpine ty arguments = case Type.typeForm ty of
      Type.App f a -> typeSpine f (a : arguments)
      -- A function type is the arrow applied to two types.
      Type.Fun a b | null arguments -> (Type.Con (NameId (-1)), [a, b])
      form -> (form, arguments)

-- | Whether the file being resolved is written with an extension.
enabled :: Extension -> M Bool
enabled extension = asks (\env -> maybe False (Set.member extension) (Map.lookup (envFile env) (envLanguages env)))

-- | The instances a data declaration asks to have derived, or one asked
-- for apart from it, given the names this module's types were given and
-- the module's declarations.
deriving' :: Map.Map Name NameId -> [Syntax.Decl] -> Syntax.Decl -> M ()
deriving' own decls d = within d $ case d of
  Decl.Data {} -> forM_ (Decl.derived d) (\(how, c) -> derive own d how Nothing (lookup c (Decl.derivedVia d)) c)
  Decl.StandaloneDeriving how hd -> case classHead hd of
    Just (context, c, [target])
      | Just (t, vs) <- typedShape target,
        declared : _ <- [x | x@(Decl.Data t' _ _) <- decls, t' == t] ->
          derive own declared how (Just (vs, context)) Nothing c
    _ -> complain "an instance is derived apart from a type's declaration for a type this module declares, applied to variables"
  _ -> pure ()

-- | Derive an instance of a class for the type a declaration declares:
-- as the Report says, for its classes and the few like them; as the
-- instance of what a newtype is of; or with no methods. Which may be
-- said. Where the instance was asked for apart from the type, the
-- variables it named and the context it gave are here too.
derive :: Map.Map Name NameId -> Syntax.Decl -> Maybe Name -> Maybe ([Name], [SType.Type]) -> Maybe SType.Type -> Name -> M ()
derive own d said apart via c = case d of
  Decl.Data t parameters cs -> do
    -- Whether a field's type is the type being declared, as it is declared.
    let itself = \case
          SType.Paren inner -> itself inner
          SType.Con n -> null parameters && n == t
          SType.App (SType.Con n) arguments -> n == t && map Just parameters == map (\case SType.Var v -> Just v; _ -> Nothing) arguments
          _ -> False
    asks (Map.lookup c . envClasses) >>= \case
      -- Every type is one that can be asked for when a program runs, and
      -- to derive that it is says nothing.
      _ | unqualified c == "Typeable" -> do
        on <- enabled DeriveDataTypeable
        unless on (complain "an instance of Typeable is derived, which says nothing, only if DeriveDataTypeable is on")
      Nothing -> complain ("unknown class " ++ c)
      Just k -> do
        constructors <- fmap catMaybes . forM cs $ \(Syntax.Constructor name fields) ->
          asks (fmap (name,,length fields) . Map.lookup name . envConstructors)
        -- Only a parameter that a field mentions is asked to be an instance.
        -- A constructor given by its type has its own names for them.
        let named con v = case Syntax.constructorResult con >>= typedShape of
              Just (_, vs) | Just p <- lookup v (zip vs parameters) -> p
              _ -> v
            mentioned = concat [map (named con) (concatMap typeVariables tys) | con@(Syntax.Constructor _ tys) <- cs]
            constrained = filter (`elem` mentioned) (nub parameters)
        allowed <- enabled EmptyDataDeriving
        strategies <- enabled DerivingStrategies
        when (isJust said && not strategies) (complain "how an instance is derived is said only if DerivingStrategies is on")
        generalised <- enabled GeneralisedNewtypeDeriving
        anyclass <- enabled DeriveAnyClass
        -- Left unsaid, it is as the Report derives where the class is one
        -- that can be; and failing that with no methods, or as a
        -- newtype's is, whichever is asked for, and as GHC does the first
        -- if both are. Where newtypes' instances are asked for, a newtype
        -- has those of the Report's classes too that are of its values
        -- and not of how it is written: all but Read and Show.
        let stock = unqualified c `elem` ["Eq", "Ord", "Show", "Read", "Enum", "Bounded", "Ix", "Functor", "Foldable", "Traversable", "Generic"]
            how = case said of
              Just s -> s
              Nothing
                | Decl.isNewtype d && generalised && unqualified c `elem` ["Eq", "Ord", "Enum", "Bounded", "Ix"] -> "newtype"
                | stock -> "stock"
                | anyclass -> "anyclass"
                | Decl.isNewtype d && generalised -> "newtype"
                | otherwise -> "stock"
            -- A type that takes a parameter may be a functor in its last.
            last' = case (reverse (nub parameters), cs) of
              (a : _, _) -> Just (a, [(prefixed name, [(maybe a (\(_, vs) -> last (a : vs)) (Syntax.constructorResult con >>= typedShape), ty) | ty <- tys]) | con@(Syntax.Constructor name tys) <- cs])
              ([], _) -> Nothing
            prefixed name = if take 1 name == ":" then "(" ++ name ++ ")" else name
            functorial extension make = do
              on <- enabled extension
              unless on (complain ("an instance of " ++ c ++ " is derived only if " ++ show extension ++ " is on"))
              case last' >>= \(_, shapes) -> make shapes of
                Just equations -> written ("instance " ++ functorContext c parameters cs ++ c ++ " (" ++ unwords (t : reverse (drop 1 (reverse (nub parameters)))) ++ ") where { " ++ intercalate "; " equations ++ " }")
                Nothing -> complain ("an instance of " ++ c ++ " cannot be derived for " ++ t ++ ": it is not a functor in its last parameter")
        methods <- case unqualified c of
          _ | how == "anyclass" -> do
            variables <- mapM (`newName` TypeVariable) (nub parameters)
            n <- node
            at' <- appliedTo (own Map.! t) variables
            more <- defaultedFor k []
            Nothing <$ addInstance (Instance n k [at'] variables [] more Exactly)
          _ | how == "newtype" -> Nothing <$ newtypeDerived own d apart Nothing k
          -- By way of another type: as a newtype's instance is of what
          -- the newtype is of, but of the type that is named.
          _ | how == "via" -> Nothing <$ newtypeDerived own d apart via k
          -- A type with no constructors has no values to tell apart, to
          -- show or to read.
          _ | null cs, not allowed -> Nothing <$ complain "an instance can be derived for a type with no constructors only with EmptyDataDeriving"
          "Generic" -> Nothing <$ derivedGeneric d
          "Functor" -> Nothing <$ functorial DeriveFunctor functorEquations
          "Foldable" -> Nothing <$ functorial DeriveFoldable foldableEquations
          "Traversable" -> Nothing <$ functorial DeriveTraversable traversableEquations
          "Eq" | null cs -> Nothing <$ written (writtenHead c t (nub parameters) constrained ++ "{ _ == _ = True }")
          "Ord" | null cs -> Nothing <$ written (writtenHead c t (nub parameters) constrained ++ "{ compare _ _ = EQ }")
          "Show" | null cs -> Nothing <$ written (writtenHead c t (nub parameters) constrained ++ "{ showsPrec _ v = seq v id }")
          "Read" | null cs -> Nothing <$ written (writtenHead c t (nub parameters) constrained ++ "{ readsPrec _ _ = [] }")
          "Eq" -> Just . pure <$> derivedEq constructors
          "Ord" -> Just . pure <$> derivedOrd constructors
          "Show" -> Just . pure <$> derivedShow (zip3 constructors (map Syntax.constructorFields cs) (map Syntax.constructorInfix cs))
          "Enum"
            | all (\(_, _, arity) -> arity == 0) constructors && not (null constructors) -> Just <$> derivedEnum constructors
            | otherwise -> Nothing <$ complain "Enum can only be derived for a type whose constructors have no fields"
          "Bounded"
            | all (\(_, _, arity) -> arity == 0) constructors && not (null constructors) || length constructors == 1 -> Just <$> derivedBounded constructors
            | otherwise -> Nothing <$ complain "Bounded can only be derived for a type with one constructor, or whose constructors have no fields"
          "Read" -> do
            shapes <- forM cs $ \con@(Syntax.Constructor name tys) ->
              (\(_, prec) -> (name, length tys, Syntax.constructorFields con, if Syntax.constructorInfix con then Just prec else Nothing, map itself tys)) <$> fixityOf name
            Nothing <$ written (readInstance c t (nub parameters) constrained shapes)
          "Ix"
            | all (\(_, _, arity) -> arity == 0) constructors && not (null constructors) || length constructors == 1 ->
                Nothing <$ written (ixInstance c t (nub parameters) constrained [(name, arity) | (name, _, arity) <- constructors])
            | otherwise -> Nothing <$ complain "Ix can only be derived for a type with one constructor, or whose constructors have no fields"
          _ -> Nothing <$ complain ("instances of " ++ c ++ " cannot be derived")
        forM_ methods $ \bs -> do
          variables <- mapM (`newName` TypeVariable) (nub parameters)
          context <- forM [v | (p, v) <- zip (nub parameters) variables, p `elem` constrained] (constraintOn k)
          n <- node
          at' <- appliedTo (own Map.! t) variables
          addInstance (Instance n k [at'] variables context bs Exactly)
  _ -> pure ()

-- | The context of a derived instance of a class of functors: the class
-- for each type variable that is applied to something in a field.
functorContext :: Name -> [Name] -> [Syntax.Constructor] -> String
functorContext c parameters cs = case nub (concat [heads ty | Syntax.Constructor _ tys <- cs, ty <- tys]) of
  [] -> ""
  hs -> "(" ++ intercalate ", " [c ++ " " ++ h | h <- hs] ++ ") => "
  where
    heads = \case
      SType.App f arguments -> [h | SType.Var h <- [f], h `elem` parameters] ++ concatMap heads (f : arguments)
      SType.Fun a b -> heads a ++ heads b
      SType.Tuple ts -> concatMap heads ts
      SType.List e -> heads e
      SType.Paren inner -> heads inner
      _ -> []

-- | How a type that may mention a variable is gone over, as one of three
-- things is done at the variable: a function for the type, written out,
-- or nothing if the type does not mention the variable; or failure, if
-- the variable is where it cannot be reached. Given what is done at the
-- variable, at something a type constructor is applied to, in a tuple,
-- and behind a function, which may not be allowed.
over :: Name -> String -> (String -> String) -> ([Maybe String] -> Maybe String) -> Maybe (String -> String) -> SType.Type -> Maybe (Maybe String)
over a at inside tuple behind = go
  where
    go ty
      | a `notElem` typeVariables ty = Just Nothing
      | otherwise = case ty of
          SType.Var _ -> Just (Just at)
          SType.Paren inner -> go inner
          SType.List element -> fmap inside <$> go element
          SType.Tuple ts -> tuple <$> mapM go ts
          SType.Fun from to | a `notElem` typeVariables from, Just wrap <- behind -> fmap wrap <$> go to
          SType.App f arguments
            | a `notElem` concatMap typeVariables (f : drop 1 (reverse arguments)), last' : _ <- reverse arguments -> fmap inside <$> go last'
          _ -> Nothing

-- | The names a generated function gives the parts of a tuple.
parts :: [a] -> [String]
parts xs = ["t" ++ show i | i <- [1 .. length xs]]

-- | A derived fmap: each constructor, with the function applied wherever
-- the last parameter is. A constructor is its name and its fields, each
-- its name for that parameter and its type.
functorEquations :: [(String, [(Name, SType.Type)])] -> Maybe [String]
functorEquations [] = Just ["fmap _ v = seq v undefined"]
functorEquations shapes = forM shapes $ \(con, fields) -> do
  mapped <- forM fields (\(a, ty) -> over a "f" (\m -> "(fmap " ++ m ++ ")") tuple (Just (\m -> "((.) " ++ m ++ ")")) ty)
  let xs = ["x" ++ show i | i <- [1 .. length fields]]
  Just ("fmap f (" ++ unwords (con : xs) ++ ") = " ++ unwords (con : [maybe x (\m -> "(" ++ m ++ " " ++ x ++ ")") m | (x, m) <- zip xs mapped]))
  where
    tuple ms = Just ("(\\(" ++ intercalate ", " (parts ms) ++ ") -> (" ++ intercalate ", " [maybe x (\m -> m ++ " " ++ x) m | (x, m) <- zip (parts ms) ms] ++ "))")

-- | A derived foldr: the function applied to each of the last parameter's
-- values, from the right.
foldableEquations :: [(String, [(Name, SType.Type)])] -> Maybe [String]
foldableEquations [] = Just ["foldr _ z _ = z"]
foldableEquations shapes = forM shapes $ \(con, fields) -> do
  folded <- forM fields (\(a, ty) -> over a "f" (\m -> "(\\c z' -> foldr " ++ m ++ " z' c)") tuple Nothing ty)
  let xs = ["x" ++ show i | i <- [1 .. length fields]]
  Just ("foldr f z (" ++ unwords (con : xs) ++ ") = " ++ nest "z" (zip xs folded))
  where
    nest = foldr (\(x, m) rest -> maybe rest (\m' -> m' ++ " " ++ x ++ " (" ++ rest ++ ")") m)
    tuple ms = Just ("(\\(" ++ intercalate ", " (parts ms) ++ ") z' -> " ++ nest "z'" (zip (parts ms) ms) ++ ")")

-- | A derived traverse: the constructor again, of what the action gives
-- for each of the last parameter's values, from the left.
traversableEquations :: [(String, [(Name, SType.Type)])] -> Maybe [String]
traversableEquations [] = Just ["traverse _ v = seq v undefined"]
traversableEquations shapes = forM shapes $ \(con, fields) -> do
  traversed <- forM fields (\(a, ty) -> over a "f" (\m -> "(traverse " ++ m ++ ")") tuple Nothing ty)
  let xs = ["x" ++ show i | i <- [1 .. length fields]]
  Just ("traverse f (" ++ unwords (con : xs) ++ ") = " ++ chain con (zip xs traversed))
  where
    chain con fields = "pure " ++ con ++ concat [" <*> " ++ maybe ("pure " ++ x) (\m -> m ++ " " ++ x) m | (x, m) <- fields]
    tuple ms = Just ("(\\(" ++ intercalate ", " (parts ms) ++ ") -> " ++ chain ("(" ++ replicate (length ms - 1) ',' ++ ")") (zip (parts ms) ms) ++ ")")

-- | An instance for a newtype that is the instance of the type it is a
-- newtype of: each method is that type's, taken as this one's, which it
-- can be since a newtype and what it is of are kept the same way. The
-- instance asks of the newtype's parameters what it would ask if derived
-- as the Report derives, unless a context was given with the request.
newtypeDerived :: Map.Map Name NameId -> Syntax.Decl -> Maybe ([Name], [SType.Type]) -> Maybe SType.Type -> NameId -> M ()
newtypeDerived own d apart via k = case d of
  Decl.Data t parameters cs | isJust via || (Decl.isNewtype d && length cs == 1) -> do
    variables <- mapM (`newName` TypeVariable) (nub parameters)
    let scope = zip (nub parameters) variables
    -- The type whose instance it is: the one named, or what the newtype
    -- is a newtype of.
    whole <- case (via, cs) of
      (Just named, _) -> withTypeVariables scope (type' named)
      (Nothing, con@(Syntax.Constructor _ (field : _)) : _) -> do
        scope' <- maybe (pure scope) (typedScope t scope) (Syntax.constructorResult con)
        withTypeVariables scope' (type' field)
      _ -> (`Type.Type` Type.Error) <$> node
    cls <- gets (IntMap.lookup (nameKey k) . classes)
    known <- gets signatures
    let methods = [(m, s) | m <- maybe [] classMethods cls, Just s <- [IntMap.lookup (nameKey m) known]]
        -- A class of type constructors is one whose variable is applied
        -- to something: then the instance is for the newtype without its
        -- last parameter, and of what it is of without that too.
        higher = or [applies a (signatureType s) | (_, s@(Signature (a : _) _ _)) <- methods]
        reduced = case (reverse variables, Type.typeForm whole) of
          (lastV : _, Type.App f (Type.Type _ (Type.Var v))) | v == lastV, not (mentions lastV f) -> Just f
          _ -> Nothing
    case (higher, reduced) of
      (True, Nothing) -> complain "an instance of a class of type constructors is derived for a newtype of something applied to its last parameter, and to nothing else of that parameter"
      _ -> do
        let of' = if higher then fromMaybe whole reduced else whole
            instanceVariables' = if higher then take (length variables - 1) variables else variables
        bs <- forM methods $ \(m, s) -> case s of
          Signature (a : others) context ty -> do
            ty' <- substituted a of' ty
            context' <- forM context (\(Constraint c' us) -> Constraint c' <$> mapM (substituted a of') us)
            taken <- mk (Var m) >>= \inner -> mk (Annotated inner (Signature others context' ty'))
            body <- mk (Prim Coerce) >>= (`apply` [taken]) >>= plain
            b <- node
            (\c' -> Binding b (Function m [Clause c' [] body])) <$> node
          _ -> error "Powder.Resolve: a method's signature does not start with its class's variable"
        -- What the instance asks of the newtype's parameters is what the
        -- instance it is taken from asks of them, as far as the instances
        -- known here say; and failing that, the class of each parameter
        -- the type mentions.
        needed <- needs k of'
        context <- case (apart, needed) of
          (Just (vs, given), _) -> withTypeVariables (zip vs variables) (concat <$> mapM constraints given)
          (Nothing, Just asked) -> forM (nub asked) (uncurry constraintOn)
          (Nothing, Nothing)
            | higher -> pure []
            | otherwise -> forM [v | v <- variables, mentions v whole] (constraintOn k)
        n <- node
        at' <- appliedTo (own Map.! t) instanceVariables'
        addInstance (Instance n k [at'] instanceVariables' context bs Exactly)
  _ -> complain "an instance is derived as a newtype's only for a newtype"
  where
    mentions v ty = case Type.typeForm ty of
      Type.Var x -> x == v
      Type.App f x -> mentions v f || mentions v x
      Type.Fun a b -> mentions v a || mentions v b
      _ -> False
    applies v ty = case Type.typeForm ty of
      Type.App f x -> (case Type.typeForm f of Type.Var y -> y == v; _ -> applies v f) || applies v x
      Type.Fun a b -> applies v a || applies v b
      _ -> False

-- | What a type's being an instance of a class comes to, of the type's
-- variables: the classes each must be an instance of, as the instances
-- known so far say. Nothing where they do not say, which is so for an
-- instance declared further down.
needs :: NameId -> Type.Type -> M (Maybe [(NameId, NameId)])
needs cls whole = do
  known <- gets instances
  aliases <- gets synonyms
  let go :: Int -> NameId -> Type.Type -> Maybe [(NameId, NameId)]
      go depth c ty
        | depth > 32 = Nothing
        | otherwise = case Type.typeForm ty of
            Type.Var v -> Just [(c, v)]
            _ -> case spine ty [] of
              (Just t, arguments)
                | Just (Synonym parameters body) <- IntMap.lookup (nameKey t) aliases ->
                    if length parameters == length arguments then go (depth + 1) c (replaced (zip parameters arguments) body) else Nothing
                -- An instance at the constructor applied to variables.
                | (inst, vs) : _ <- [(i, vs) | i <- known, instanceClass i == c, [hd] <- [instanceHead i], (Just t', vs) <- [variablesOf hd], t' == t, length vs == length arguments] ->
                    concat <$> forM (instanceContext inst) (\case
                      Constraint c' [u] -> go (depth + 1) c' (replaced (zip vs arguments) u)
                      _ -> Nothing)
              _ -> Nothing
      spine ty arguments = case Type.typeForm ty of
        Type.App f x -> spine f (x : arguments)
        Type.Fun a b | null arguments -> (Just (NameId (-1)), [a, b])
        Type.Con t -> (Just t, arguments)
        _ -> (Nothing, arguments)
      variablesOf hd = case spine hd [] of
        (found, arguments) -> (found, [v | Type.Var v <- map Type.typeForm arguments])
      -- Only looked at, so it keeps the nodes it has.
      replaced with ty = case Type.typeForm ty of
        Type.Var v | Just other <- lookup v with -> other
        Type.App f x -> Type.Type (Type.typeId ty) (Type.App (replaced with f) (replaced with x))
        Type.Fun a b -> Type.Type (Type.typeId ty) (Type.Fun (replaced with a) (replaced with b))
        _ -> ty
  pure (go 0 cls whole)

-- | A type with another in place of a variable, as new syntax.
substituted :: NameId -> Type.Type -> Type.Type -> M Type.Type
substituted a by ty = case Type.typeForm ty of
  Type.Var v | v == a -> substituted a by by'
    where
      -- What is put in is copied too, and has none of the variable.
      by' = by
  Type.App f x -> (Type.App <$> substituted a by f <*> substituted a by x) >>= fresh'
  Type.Fun x y -> (Type.Fun <$> substituted a by x <*> substituted a by y) >>= fresh'
  Type.Forall vs context inner
    | a `elem` vs -> fresh' (Type.Forall vs context inner)
    | otherwise -> (Type.Forall vs <$> mapM (\(c, ts) -> (,) c <$> mapM (substituted a by) ts) context <*> substituted a by inner) >>= fresh'
  form -> fresh' form
  where
    fresh' form = (`Type.Type` form) <$> node

-- | Take an instance written out here as if the source had it. What it
-- refers to is the prelude's, whatever the module has of the same names,
-- and anything wrong with it is placed at what it was written for.
written :: String -> M ()
written source = case (parsedProblems parsed, cast (Red.root (parsedTree parsed)) :: Maybe Module) of
  ([], Just m) -> local (\env -> env {envPinned = True, envValues = Map.union (envGlobals env) (envValues env)}) (mapM_ instanceDecl (moduleDecls m))
  _ -> complain "an instance derived here could not be written, which is a fault in the compiler"
  where
    parsed = parseModule source

-- | An instance of Generic: the type its values are kept as, which is
-- made of the few types of GHC.Generics and says the names of the type,
-- its constructors and their fields; and the two ways between a value
-- and that. The constructors are a sum and a constructor's fields a
-- product, each halved and halved again, as GHC has them.
derivedGeneric :: Syntax.Decl -> M ()
derivedGeneric d = case d of
  Decl.Data t parameters cs -> do
    on <- enabled DeriveGeneric
    unless on (complain "an instance of Generic is derived only if DeriveGeneric is on")
    home <- asks envModule
    strictly <- enabled StrictData
    fixities <- forM cs (\(Syntax.Constructor name _) -> fixityOf name)
    let plainly = all (\con -> isNothing (Syntax.constructorResult con) && null (Syntax.constructorExistentials con) && null (Syntax.constructorContext con)) cs
        joined :: String -> String -> (a -> String) -> [a] -> String
        joined op none one = go
          where
            go [] = none
            go [x] = one x
            go xs = let (l, r) = splitAt (length xs `div` 2) xs in "(" ++ go l ++ " " ++ op ++ " " ++ go r ++ ")"
        spelt name = if any isAlphaNum (take 1 name) then name else "(" ++ name ++ ")"
        tokensOf r = concat [if Red.isToken c then [c | not (Kind.isTrivia (Red.kind c)), not (Kind.isVirtual (Red.kind c))] else tokensOf c | c <- Red.children r]
        typeText ty = "(" ++ unwords (map (Text.unpack . Red.text) (tokensOf (syntax ty))) ++ ")"
        constructorRep (con@(Syntax.Constructor name tys), (assoc, prec)) =
          let names = Syntax.constructorFields con
              marks = Syntax.constructorMarks con
              fixity
                | Syntax.constructorInfix con =
                    "('InfixI '" ++ (case assoc of LeftAssoc -> "LeftAssociative"; RightAssoc -> "RightAssociative"; NonAssoc -> "NotAssociative") ++ " " ++ show prec ++ ")"
                | otherwise = "'PrefixI"
              selector (i, ty) =
                let field = case drop i names of
                      f : _ -> "('Just " ++ show f ++ ")"
                      [] -> "'Nothing"
                    (said, decided) = case drop i marks of
                      Just True : _ -> ("'SourceStrict", "'DecidedStrict")
                      Just False : _ -> ("'SourceLazy", "'DecidedLazy")
                      _ -> ("'NoSourceStrictness", if strictly then "'DecidedStrict" else "'DecidedLazy")
                 in "S1 ('MetaSel " ++ field ++ " 'NoSourceUnpackedness " ++ said ++ " " ++ decided ++ ") (Rec0 " ++ typeText ty ++ ")"
           in "C1 ('MetaCons " ++ show name ++ " " ++ fixity ++ " " ++ (if null names then "'False" else "'True") ++ ") (" ++ joined ":*:" "U1" selector (zip [0 :: Int ..] tys) ++ ")"
        rep = "D1 ('MetaData " ++ show t ++ " " ++ show home ++ " \"main\" " ++ (if Decl.isNewtype d then "'True" else "'False") ++ ") (" ++ joined ":+:" "V1" constructorRep (zip cs fixities) ++ ")"
        variables n = ["a" ++ show i | i <- [1 .. n]]
        inject :: Int -> Int -> String -> String
        inject i n e
          | n <= 1 = e
          | i < n `div` 2 = "L1 (" ++ inject i (n `div` 2) e ++ ")"
          | otherwise = "R1 (" ++ inject (i - n `div` 2) (n - n `div` 2) e ++ ")"
        alternatives build =
          intercalate
            "; "
            [ build (unwords (spelt name : variables (length tys))) (inject i (length cs) ("M1 (" ++ joined ":*:" "U1" (\v -> "M1 (K1 " ++ v ++ ")") (variables (length tys)) ++ ")"))
              | (i, Syntax.Constructor name tys) <- zip [0 ..] cs
            ]
        whole = "(" ++ unwords (spelt t : nub parameters) ++ ")"
        text =
          "instance Generic " ++ whole ++ " where { type Rep " ++ whole ++ " = " ++ rep
            ++ "; from x = M1 (case x of { " ++ alternatives (\value kept -> value ++ " -> " ++ kept) ++ " })"
            ++ "; to (M1 r) = case r of { " ++ alternatives (\value kept -> kept ++ " -> " ++ value) ++ " } }"
    if plainly
      then writtenGenerically text
      else complain ("an instance of Generic cannot be derived for " ++ t ++ ": a constructor of it has type variables or a context of its own, or is given by its type")
  _ -> pure ()

-- | Resolve an instance of Generic written here. What it is written with
-- is GHC.Generics's, whatever the module has of the same names, and it
-- is read with what extensions it is written with.
writtenGenerically :: String -> M ()
writtenGenerically source = case (parsedProblems parsed, cast (Red.root (parsedTree parsed)) :: Maybe Module) of
  ([], Just m) -> do
    generics <- asks envGenerics
    let one = Map.mapMaybe (\case [x] -> Just x; _ -> Nothing)
        the names = (`Map.restrictKeys` Set.fromList names) . one
        types = the ["D1", "C1", "S1", "Rec0", "U1", "V1", ":+:", ":*:", "Rep"] (scopeTypes generics)
        constructors = the ["M1", "K1", "U1", "L1", "R1", ":*:", "MetaData", "MetaCons", "MetaSel", "PrefixI", "InfixI", "LeftAssociative", "RightAssociative", "NotAssociative", "NoSourceUnpackedness", "NoSourceStrictness", "SourceLazy", "SourceStrict", "DecidedLazy", "DecidedStrict"] (scopeConstructors generics)
        values = the ["from", "to"] (scopeValues generics)
    local
      ( \env ->
          env
            { envPinned = True,
              envValues = Map.union values (Map.union (envGlobals env) (envValues env)),
              envOwnValues = Map.union values (envOwnValues env),
              envTypes = Map.union types (envTypes env),
              envConstructors = Map.union constructors (envConstructors env),
              envClasses = Map.union (the ["Generic"] (scopeClasses generics)) (envClasses env),
              envLanguages = Map.insertWith Set.union (envFile env) needed (envLanguages env)
            }
      )
      (forM_ (moduleDecls m) (\d -> forM_ [ds | Decl.Instance _ ds <- [d]] (mapM_ familyEquation) >> instanceDecl d))
  _ -> complain "an instance of Generic derived here could not be written, which is a fault in the compiler"
  where
    needed = Set.fromList [DataKinds, TypeOperators, TypeFamilies, KindSignatures, EmptyCase, FlexibleInstances]
    parsed = parseModuleWith needed source

-- | An instance's head and context, each parameter of the type being
-- asked to be an instance too.
writtenHead :: Name -> Name -> [Name] -> [Name] -> String
writtenHead cls t parameters constrained = "instance " ++ context ++ cls ++ " (" ++ unwords (t : parameters) ++ ") where "
  where
    context = if null constrained then "" else "(" ++ intercalate ", " [cls ++ " " ++ a | a <- constrained] ++ ") => "

-- | Read as 'Show' would have shown: a constructor's name and then its
-- fields, in parentheses where it is an argument; an operator between
-- its two; a record's fields by name. Each constructor is its name, how
-- many fields it has, their names if it is a record's, and how tightly
-- it binds if it is an operator.
-- | An instance of Read as the Report's section 11.4 has it, written out.
--
-- The Report gives each constructor its own reader, in its own optional
-- parentheses, and joins them. Done as written that reads what is in
-- parentheses once for every constructor, and what is to the left of an
-- operator once for every operator, at every depth: a value nested a few
-- deep takes minutes. So here the same parses are found with the work
-- shared. Whatever is in parentheses is read once, as any of the type's
-- values, which is what each constructor's parentheses come to together;
-- and an operator's left operand, where it is of the type being read, is
-- read by this reader and not by starting again. What a constructor's
-- reader finds does not depend on what is around it, only whether it is
-- wanted does, so each is found once and kept. (The combinator reducer
-- would keep them anyway: it shares whatever does not depend on a
-- function's argument. No other machine does.)
--
-- A constructor is its name, how many fields it has, their names if they
-- have them, its precedence if it is written between its fields, and
-- which of its fields are of the type itself.
readInstance :: Name -> Name -> [Name] -> [Name] -> [(Name, Int, [Name], Maybe Int, [Bool])] -> String
readInstance cls t parameters constrained constructors =
  writtenHead cls t parameters constrained
    ++ "{ readsPrec d r = go d where { lexed = lex r; "
    ++ "wrapped = [(x, s2) | (\"(\", s0) <- lexed, (x, s1) <- readsPrec 0 s0, (\")\", s2) <- lex s1]; "
    ++ concat [parses i ++ " = " ++ made ++ "; " | (i, (_, made)) <- alternatives]
    ++ "go k = "
    ++ intercalate " ++ " ([maybe (parses i) (\prec -> "(if k > " ++ show prec ++ " then [] else " ++ parses i ++ ")") most | (i, (most, _)) <- alternatives] ++ ["wrapped"])
    ++ " } }"
  where
    -- What each constructor's reader finds is found once, whatever is
    -- around: only whether it is wanted depends on that.
    alternatives = zip [1 :: Int ..] (map alternative constructors)
    parses i = "parses" ++ show i
    x i = "x" ++ show (i :: Int)
    symbolic name = take 1 name == ":" || not (any (`elem` ['a' .. 'z'] ++ ['A' .. 'Z'] ++ "_") (take 1 name))
    -- A name as lexemes: an operator where a name is wanted is in
    -- parentheses, and a name where an operator is wanted in backquotes.
    prefixed name = if symbolic name then [Left "(", Left name, Left ")"] else [Left name]
    between name = if symbolic name then [Left name] else [Left "`", Left name, Left "`"]
    alternative (name, arity, fieldNames, infixed, own)
      | arity == 0 = reading Nothing (written' name) (prefixed name)
      | Just prec <- infixed,
        arity == 2 =
          reading (Just prec) (x 1 ++ " " ++ (if symbolic name then name else "`" ++ name ++ "`") ++ " " ++ x 2) ([Right (prec + 1, 1, take 1 own == [True])] ++ between name ++ [Right (prec + 1, 2, False)])
      | not (null fieldNames) =
          let field i f = [Left (if i == 1 then "{" else ",")] ++ prefixed f ++ [Left "=", Right (0, i, False)]
           in reading (Just 11) (unwords (written' name : map x [1 .. arity])) (prefixed name ++ concat (zipWith field [1 ..] fieldNames) ++ [Left "}"])
      | otherwise = reading (Just 10) (unwords (written' name : map x [1 .. arity])) (prefixed name ++ [Right (11, i, False) | i <- [1 .. arity]])
    written' name = if symbolic name then "(" ++ name ++ ")" else name
    -- A reader of lexemes and values in a row: each step reads from where
    -- the one before it left off, the first from the start. It reads
    -- nothing where what is around binds tighter than the constructor.
    reading :: Maybe Int -> String -> [Either String (Int, Int, Bool)] -> (Maybe Int, String)
    reading most made steps =
      (most, "[(" ++ made ++ ", s" ++ show (length steps) ++ ") | " ++ intercalate ", " (zipWith step [0 :: Int ..] steps) ++ "]")
    step i = \case
      Left lexeme -> "(" ++ show lexeme ++ ", s" ++ show (i + 1) ++ ") <- " ++ (if i == 0 then "lexed" else "lex s" ++ show i)
      Right (prec, v, mine) ->
        "(" ++ x v ++ ", s" ++ show (i + 1) ++ ") <- "
          ++ (if i == 0 then (if mine then "go " ++ show (prec :: Int) else "readsPrec " ++ show prec ++ " r") else "readsPrec " ++ show prec ++ " s" ++ show i)

-- | Indexed in the order the constructors were declared; or for the one
-- constructor of a type, by its fields, the last counting fastest.
ixInstance :: Name -> Name -> [Name] -> [Name] -> [(Name, Int)] -> String
ixInstance cls t parameters constrained constructors = writtenHead cls t parameters constrained ++ "{ " ++ intercalate "; " methods ++ " }"
  where
    methods = case constructors of
      [(k, arity)] | arity > 0 ->
        let at c = unwords (k : [c ++ show i | i <- [1 .. arity]])
            both i = "(l" ++ show i ++ ", u" ++ show i ++ ")"
            bounds' = "(" ++ at "l" ++ ", " ++ at "u" ++ ")"
         in [ "range " ++ bounds' ++ " = [" ++ at "i" ++ " | " ++ intercalate ", " ["i" ++ show i ++ " <- range " ++ both i | i <- [1 .. arity]] ++ "]",
              "index " ++ bounds' ++ " (" ++ at "i" ++ ") = " ++ foldl (\acc i -> "(" ++ acc ++ ") * rangeSize " ++ both i ++ " + index " ++ both i ++ " i" ++ show i) "0" [1 .. arity],
              "inRange " ++ bounds' ++ " (" ++ at "i" ++ ") = " ++ intercalate " && " ["inRange " ++ both i ++ " i" ++ show i | i <- [1 .. arity]]
            ]
      _ ->
        let place = " where { place v = case v of { " ++ intercalate "; " [k ++ " -> " ++ show i | (i, (k, _)) <- zip [0 :: Int ..] constructors] ++ " } }"
         in [ "range (a, b) = [c | c <- [" ++ intercalate ", " (map fst constructors) ++ "], inRange (a, b) c]",
              "index (a, b) x = if inRange (a, b) x then place x - place a else error \"Ix.index: index out of range\"" ++ place,
              "inRange (a, b) x = place a <= place x && place x <= place b" ++ place
            ]

-- | A method defined by equations, each its patterns and what it is then.
equations :: Name -> [([Pat], Expr)] -> M Binding
equations method clauses = do
  found <- asks (Map.lookup method . envGlobals)
  case found of
    Nothing -> complain (method ++ " is not defined, and deriving this needs it") >> (`Binding` Function (NameId (-1)) []) <$> node
    Just m -> do
      n <- node
      Binding n . Function m <$> forM clauses (\(ps, body) -> Clause <$> node <*> pure ps <*> plain body)

-- | A right-hand side that is just an expression.
plain :: Expr -> M Rhs
plain body = do
  r <- node
  g <- node
  pure (Rhs r [Guarded g [] body] [])

-- | A constructor applied to new variables, as a pattern, with them.
built :: NameId -> Int -> M (Pat, [NameId])
built c arity = do
  xs <- replicateM arity (newName "x" Local)
  p <- mapM (mkPat . Pat.Var) xs >>= mkPat . Pat.Con c
  pure (p, xs)

call :: Name -> [Expr] -> M Expr
call f arguments = builtin f >>= (`apply` arguments)

-- | Equal if built by the same constructor from equal fields.
derivedEq :: [(Name, NameId, Int)] -> M Binding
derivedEq constructors = do
  same <- forM constructors $ \(_, c, arity) -> do
    (p, xs) <- built c arity
    (q, ys) <- built c arity
    fields <- zipWithM (\x y -> sequence [mk (Var x), mk (Var y)] >>= call "==") xs ys
    body <- case reverse fields of
      [] -> construct "True"
      final : before -> foldM (\rest f -> call "&&" [f, rest]) final before
    pure ([p, q], body)
  different <- if length constructors < 2 then pure [] else pure <$> ((,) <$> replicateM 2 (mkPat Pat.Wild) <*> construct "False")
  equations "==" (same ++ different)

-- | Ordered by constructor, in the order they were declared, and then by
-- fields, the first that differ deciding.
derivedOrd :: [(Name, NameId, Int)] -> M Binding
derivedOrd constructors = do
  same <- forM constructors $ \(_, c, arity) -> do
    (p, xs) <- built c arity
    (q, ys) <- built c arity
    equal <- construct "EQ"
    body <- foldM (\rest (x, y) -> thenBy x y rest) equal (reverse (zip xs ys))
    pure ([p, q], body)
  different <-
    if length constructors < 2
      then pure []
      else do
        a <- newName "a" Local
        b <- newName "b" Local
        ps <- mapM (mkPat . Pat.Var) [a, b]
        body <- mapM (positionOf constructors) [a, b] >>= call "compare"
        pure [(ps, body)]
  equations "compare" (same ++ different)
  where
    -- Compare two fields, and go on to the rest if they are equal.
    thenBy x y rest = do
      scrutinee <- sequence [mk (Var x), mk (Var y)] >>= call "compare"
      other <- newName "o" Local
      eq <- constructor "EQ"
      equalPat <- mkPat (maybe Pat.Error (`Pat.Con` []) eq)
      otherPat <- mkPat (Pat.Var other)
      alts <- sequence [Alt <$> node <*> pure equalPat <*> plain rest, Alt <$> node <*> pure otherPat <*> (mk (Var other) >>= plain)]
      mk (Case scrutinee alts)

-- | Where a value's constructor comes among its type's.
positionOf :: [(Name, NameId, Int)] -> NameId -> M Expr
positionOf constructors x = do
  scrutinee <- mk (Var x)
  alts <- forM (zip [0 ..] constructors) $ \(i, (_, c, arity)) -> do
    p <- replicateM arity (mkPat Pat.Wild) >>= mkPat . Pat.Con c
    Alt <$> node <*> pure p <*> (mk (Lit (Number i)) >>= plain)
  mk (Case scrutinee alts)

-- | Numbered in the order the constructors were declared, from nothing,
-- and running to the last of them.
derivedEnum :: [(Name, NameId, Int)] -> M [Binding]
derivedEnum constructors = do
  let final = toInteger (length constructors) - 1
      number n = mk (Lit (Number n))
      onOne f = do
        x <- newName "x" Local
        p <- mkPat (Pat.Var x)
        body <- f x
        pure [([p], body)]
      ordinal x = mk (Var x) >>= call "fromEnum" . pure
  from <- equations "fromEnum" =<< onOne (positionOf constructors)
  to <- equations "toEnum" =<< forM (zip [0 ..] constructors) (\(i, (_, c, _)) -> (,) . pure <$> mkPat (Pat.Lit (Number i)) <*> mk (Con c))
  enumFrom' <- equations "enumFrom" =<< onOne (\x -> do
    range <- sequence [ordinal x, number final] >>= call "enumFromTo"
    sequence [builtin "toEnum", pure range] >>= call "map")
  enumFromThen' <- do
    x <- newName "x" Local
    y <- newName "y" Local
    ps <- mapM (mkPat . Pat.Var) [x, y]
    rising <- sequence [ordinal y, ordinal x] >>= call ">="
    limit <- (If rising <$> number final <*> number 0) >>= mk
    range <- sequence [ordinal x, ordinal y, pure limit] >>= call "enumFromThenTo"
    body <- sequence [builtin "toEnum", pure range] >>= call "map"
    equations "enumFromThen" [(ps, body)]
  pure [from, to, enumFrom', enumFromThen']

-- | The first constructor and the last; or for the one constructor of a
-- type, that of the least of each field and of the greatest.
derivedBounded :: [(Name, NameId, Int)] -> M [Binding]
derivedBounded constructors = forM [("minBound", take 1 constructors), ("maxBound", take 1 (reverse constructors))] $ \(bound', chosen) -> do
  body <- case chosen of
    (_, c, arity) : _ -> do
      arguments <- replicateM arity (builtin bound')
      mk (Con c) >>= (`apply` arguments)
    [] -> mk Error
  equations bound' [([], body)]

-- | Shown as it would be written, in parentheses where it is an argument.
derivedShow :: [((Name, NameId, Int), [Name], Bool)] -> M Binding
derivedShow constructors = do
  clauses <- forM constructors $ \((name, c, arity), fieldNames, infixed) -> do
    d <- newName "d" Local
    depth <- mkPat (Pat.Var d)
    (p, xs) <- built c arity
    let spelled = if any (`elem` ['A' .. 'Z']) (take 1 name) then name else "(" ++ name ++ ")"
    shownName <- mk (Lit (Text spelled)) >>= call "showString" . pure
    let text t = mk (Lit (Text t)) >>= call "showString" . pure
        compose = foldM (\f g -> call "." [f, g])
    between <- case xs of
      -- Written between its fields, as it was declared, in parentheses
      -- where what is around it binds tighter.
      [l, r] | infixed -> do
        (_, prec) <- fixityOf name
        left <- sequence [mk (Lit (Number (toInteger prec + 1))), mk (Var l)] >>= call "showsPrec"
        middle <- text (" " ++ (if take 1 name == ":" then name else "`" ++ name ++ "`") ++ " ")
        right <- sequence [mk (Lit (Number (toInteger prec + 1))), mk (Var r)] >>= call "showsPrec"
        whole <- compose left [middle, right]
        tight <- sequence [mk (Var d), mk (Lit (Number (toInteger prec)))] >>= call ">"
        Just <$> call "showParen" [tight, whole]
      _ -> pure Nothing
    body <-
      if null xs
        then pure shownName
        else
          if not (null fieldNames)
            then do
              -- A record is shown as it would be written, fields named.
              parts <- forM (zip3 [0 :: Int ..] fieldNames xs) $ \(i, f, x) -> do
                -- A field that is an operator is named in parentheses.
                let spelt = if any (`elem` ['a' .. 'z'] ++ "_") (take 1 f) then f else "(" ++ f ++ ")"
                label <- text ((if i == 0 then " {" else ", ") ++ spelt ++ " = ")
                value <- sequence [mk (Lit (Number 0)), mk (Var x)] >>= call "showsPrec"
                call "." [label, value]
              closing <- text "}"
              whole <- compose shownName (parts ++ [closing])
              tight <- sequence [mk (Var d), mk (Lit (Number 10))] >>= call ">"
              call "showParen" [tight, whole]
            else do
              fields <- forM xs $ \x -> do
                space <- mk (Lit (Text " ")) >>= call "showString" . pure
                field <- sequence [mk (Lit (Number 11)), mk (Var x)] >>= call "showsPrec"
                call "." [space, field]
              whole <- foldM (\f g -> call "." [f, g]) shownName fields
              tight <- sequence [mk (Var d), mk (Lit (Number 10))] >>= call ">"
              call "showParen" [tight, whole]
    pure ([depth, p], fromMaybe body between)
  equations "showsPrec" clauses

-- Bindings ------------------------------------------------------------------

-- | Declarations as they bind: the adjacent equations of one function
-- together, and each pattern binding by itself.
data Group
  = Equations Syntax.Decl Name [(Syntax.Decl, [SPat.Pat], Syntax.Rhs)]
  | Destructuring Syntax.Decl SPat.Pat Syntax.Rhs

grouped :: [Syntax.Decl] -> [Group]
grouped ds = map gather (NE.groupBy sameFunction (concatMap binds ds))
  where
    binds d = case d of
      Decl.Clause f ps r -> [Left (d, f, ps, r)]
      Decl.PatBind p r -> [Right (d, p, r)]
      -- Signatures, fixities and types are gathered apart from what
      -- binds.
      Decl.Signature _ _ -> []
      Decl.Fixity {} -> []
      Decl.Data {} -> []
      Decl.TypeSynonym {} -> []
      Decl.Class {} -> []
      Decl.Instance {} -> []
      Decl.Import {} -> []
      Decl.Default _ -> []
      Decl.Foreign {} -> []
      Decl.ForeignExport {} -> []
      Decl.PatternSynonym {} -> []
      Decl.PatternSignature -> []
      Decl.StandaloneDeriving {} -> []
      Decl.KindSignature {} -> []
      Decl.RoleAnnotation -> []
      Decl.TypeFamily {} -> []
      Decl.TypeInstance {} -> []
      Decl.DefaultSignature {} -> []
    sameFunction (Left (_, f, _, _)) (Left (_, g, _, _)) = f == g
    sameFunction _ _ = False
    gather group = case NE.head group of
      Left (d, f, _, _) -> Equations d f [(d', ps, r) | Left (d', _, ps, r) <- NE.toList group]
      Right (d, p, r) -> Destructuring d p r

-- | The names a group brings into scope.
defines :: Group -> [Name]
defines = definesWith (const [])

-- | The same, given the fields of each constructor, which a pattern that
-- ends in two dots binds.
definesWith :: (Name -> [(Int, Name)]) -> Group -> [Name]
definesWith wild = \case
  Equations _ f _ -> [f]
  Destructuring _ p _ -> patternNames wild p

-- | The variables a pattern binds, as they are spelled.
patternNames :: (Name -> [(Int, Name)]) -> SPat.Pat -> [Name]
patternNames wild = \case
  SPat.Var x -> [x]
  SPat.As x p -> x : patternNames wild p
  SPat.Con _ ps -> concatMap (patternNames wild) ps
  SPat.Infix a _ b -> patternNames wild a ++ patternNames wild b
  p@(SPat.Record c fs) ->
    concat [maybe [unqualified f] (patternNames wild) q | (f, q) <- fs]
      ++ (if SPat.recordWild p then map snd (wild c) \\ map (unqualified . fst) fs else [])
  SPat.Bang p -> patternNames wild p
  SPat.View _ p -> patternNames wild p
  SPat.Sig p _ -> patternNames wild p
  SPat.Paren p -> patternNames wild p
  SPat.Lazy p -> patternNames wild p
  SPat.Tuple ps -> concatMap (patternNames wild) ps
  SPat.List ps -> concatMap (patternNames wild) ps
  SPat.Wild -> []
  SPat.Lit _ -> []
  SPat.NegLit _ -> []

-- | Say so if a scope defines a name twice, at the later definition.
conflicts :: [Name] -> [Group] -> M ()
conflicts defined groups = forM_ (nub (defined \\ nub defined)) $ \x ->
  case [d | g <- reverse groups, x `elem` defines g, let d = declOf g] of
    d : _ -> within d (complain ("conflicting definitions for " ++ x))
    [] -> pure ()

-- | The declaration a group starts at.
declOf :: Group -> Syntax.Decl
declOf = \case
  Equations d _ _ -> d
  Destructuring d _ _ -> d

-- | Local definitions, all in scope in each other, and something resolved
-- in their scope; with the names of what the bindings marked strict among
-- them bind, which whoever has the bindings is to work out first.
bindingsForced :: [Syntax.Decl] -> M a -> M ([Binding], [NameId], a)
bindingsForced ds inner = do
  wild <- fieldsOfConstructor
  let groups = grouped ds
      defined = concatMap (definesWith wild) groups
  conflicts defined groups
  scope <- forM (nub defined) (\x -> (,) x <$> newName x Local)
  local (\env -> env {envValues = Map.union (Map.fromList scope) (envValues env)}) $ do
    declareFixities (maybeToList . (`lookup` scope)) ds
    declaredTypesOf [] defined ds
    outer <- gets forced
    modify (\st -> st {forced = []})
    bs <- mapM binding groups
    strictly <- gets forced
    modify (\st -> st {forced = outer})
    (,,) bs strictly <$> inner

-- | A group whose names are already in scope.
binding :: Group -> M Binding
binding = bindingAs Nothing

-- | A group, which if it is the equations of something may be told what
-- that is and not look for it by name: an instance's method is the
-- class's, under whatever name that is in scope.
bindingAs :: Maybe NameId -> Group -> M Binding
bindingAs known = \case
  Equations d f clauses -> within d $ do
    name <- maybe (binder f) pure known
    when (length (nub [length ps | (_, ps, _) <- clauses]) > 1) $
      complain ("the equations for " ++ f ++ " have different numbers of arguments")
    n <- node
    Binding n . Function name <$> scopedBy name (forM clauses (\(d', ps, r) -> within d' (clause ps r)))
  Destructuring d p r -> within d $ do
    -- The names are in scope already: the pattern refers to them.
    n <- node
    p' <- patternOf p
    noViews
    -- A binding marked strict is one whose value is worked out before
    -- what the binding is for: the value is given a name to do that by.
    p'' <- case Pat.patKind p' of
      Pat.Bang inner -> do
        whole <- newName "strict" Local
        modify (\st -> st {forced = forced st ++ [whole]})
        mkPat (Pat.As whole inner)
      _ -> pure p'
    Binding n . Destructure p'' <$> rhs r
  where
    clause ps r = do
      c <- node
      (ps', scope, vs) <- patternsViewed ps
      Clause c ps' . viewed vs <$> extend scope (rhs r)

extend :: [(Name, NameId)] -> M a -> M a
extend scope = local (\env -> env {envValues = Map.union (Map.fromList scope) (envValues env)})

-- | Guards and bodies, in scope of the @where@ that follows them.
rhs :: Syntax.Rhs -> M Rhs
rhs r@(Syntax.Rhs alternatives wheres) = within r $ do
  n <- node
  (bs, strictly, guardeds) <- bindingsForced wheres . forM alternatives $ \(guard', body) -> within body $ do
    g <- node
    uncurry (Guarded g) <$> qualifiers guard' (expr body)
  -- What is bound strictly is worked out before any guard is tried.
  first <- mapM forcing strictly
  pure (Rhs n [Guarded g (first ++ qs) body | Guarded g qs body <- guardeds] bs)

-- | The qualifiers of a guard, each in scope of what those before it
-- bind, and something resolved in scope of them all.
qualifiers :: [Stmt.Stmt] -> M a -> M ([Qualifier], a)
qualifiers [] inner = (,) [] <$> inner
qualifiers (s : rest) inner = within s $ case s of
  Stmt.Expr e -> do
    c <- expr e
    (qs, a) <- qualifiers rest inner
    pure (Condition c : qs, a)
  Stmt.Bind p e -> do
    e' <- expr e
    (ps, scope, vs) <- patternsViewed [p]
    (qs, a) <- extend scope (qualifiers rest inner)
    pure ([Match q e' | q <- take 1 ps] ++ vs ++ qs, a)
  Stmt.Let ds -> do
    (bs, strictly, (qs, a)) <- bindingsForced ds (qualifiers rest inner)
    first <- mapM forcing strictly
    pure (Bindings bs : first ++ qs, a)

-- | A condition that holds once a name's value has been worked out.
forcing :: NameId -> M Qualifier
forcing x = do
  v <- mk (Var x)
  true <- construct "True"
  Condition <$> call "seq" [v, true]

-- | An expression, after the values of some names have been worked out.
forcingAll :: [NameId] -> Expr -> M Expr
forcingAll xs body = foldM (\rest x -> mk (Var x) >>= \v -> call "seq" [v, rest]) body (reverse xs)

-- | A right-hand side each of whose bodies is chosen only if some
-- qualifiers hold as well as its own, which come first. What its where
-- defines may use what they bind, so it goes in with each guard.
viewed :: [Qualifier] -> Rhs -> Rhs
viewed [] r = r
viewed vs (Rhs n guardeds bs) = Rhs n [Guarded g (vs ++ [Bindings bs | not (null bs)] ++ qs) body | Guarded g qs body <- guardeds] []

-- | An expression that is itself where some qualifiers hold, and a
-- failure to match where they do not.
viewedExpr :: [Qualifier] -> Expr -> M Expr
viewedExpr [] body = pure body
viewedExpr vs body = do
  scrutinee <- construct "()"
  anything <- mkPat Pat.Wild
  r <- node
  g <- node
  a <- node
  mk (Case scrutinee [Alt a anything (Rhs r [Guarded g vs body] [])])

-- Patterns ------------------------------------------------------------------

-- | Resolve patterns that bind new names, giving the names with them. A
-- name may be bound only once among them.
-- | The same, with what the view patterns among them ask for, which is
-- to hold where the patterns match. The patterns have a variable where
-- each view pattern was.
patternsViewed :: [SPat.Pat] -> M ([Pat], [(Name, NameId)], [Qualifier])
patternsViewed ps = do
  wild <- fieldsOfConstructor
  let spelled = concatMap (patternNames wild) ps
  forM_ (nub (spelled \\ nub spelled)) $ \x ->
    forM_ (take 1 ps) (\p -> within p (complain (x ++ " is bound more than once in these patterns")))
  scope <- forM (nub spelled) (\x -> (,) x <$> newName x Local)
  ps' <- extend scope (mapM patternOf ps)
  vs <- gets views
  modify (\st -> st {views = []})
  pure (ps', scope, vs)

-- | A view pattern, given its function and how to read its pattern: a
-- variable, and a qualifier for whoever has the pattern to add, that the
-- function applied to the variable matches. A view pattern inside its
-- pattern comes after it, since it is of what that pattern binds.
viewing :: Expr -> M Pat -> M Pat
viewing function inner = do
  x <- newName "view" Local
  applied' <- mk (Var x) >>= \argument -> apply function [argument]
  outer <- gets views
  modify (\st -> st {views = []})
  q <- inner
  modify (\st -> st {views = outer ++ Match q applied' : views st})
  mkPat (Pat.Var x)

-- | Say so if a view pattern was met where there is nowhere for what it
-- asks to go.
noViews :: M ()
noViews = do
  vs <- gets views
  modify (\st -> st {views = []})
  unless (null vs) (complain "a view pattern cannot be used in a binding that is not a function's")

-- | Resolve a pattern whose variables are in scope already, as the names
-- it binds.
patternOf :: SPat.Pat -> M Pat
patternOf p = within p $ case p of
  SPat.Var x -> binder x >>= mkPat . Pat.Var
  SPat.Wild -> mkPat Pat.Wild
  -- With OverloadedStrings a string is of any type that has them, and
  -- what is matched is compared with it.
  SPat.Lit l@(Syntax.Text _) -> do
    overloaded <- enabled OverloadedStrings
    if not overloaded
      then literal l >>= mkPat . maybe Pat.Error Pat.Lit
      else do
        string <- literal l >>= mk . maybe Error Lit >>= call "fromString" . pure
        function <- builtin "==" >>= (`apply` [string])
        viewing function (constructor "True" >>= mkPat . maybe Pat.Error (`Pat.Con` []))
  SPat.Lit l -> literal l >>= mkPat . maybe Pat.Error Pat.Lit
  SPat.NegLit (Syntax.Number n) -> mkPat (Pat.Lit (Number (negate n)))
  SPat.NegLit fraction@(Syntax.Fraction _) -> literal fraction >>= mkPat . maybe Pat.Error (\case Fraction q -> Pat.Lit (Fraction (negate q)); l -> Pat.Lit l)
  SPat.NegLit _ -> complain "only a number can be negated in a pattern" >> mkPat Pat.Error
  SPat.Con c ps ->
    constructor c >>= \case
      Nothing -> mkPat Pat.Error
      Just k -> do
        before <- gets (length . views)
        mapM patternOf ps >>= applied c k before
  -- A row of patterns with constructors between them groups as the
  -- constructors' fixities say.
  SPat.Infix {} -> do
    before <- gets (length . views)
    let row q = case q of
          SPat.Infix a c b -> let (more, between) = row b in (a : more, c : between)
          _ -> ([q], [])
        (operands, operators) = row p
        climb :: Int -> [Pat] -> [Name] -> M (Pat, [Pat], [Name])
        climb least qs os = case qs of
          lhs : rest -> loop least lhs rest os
          [] -> (,,) <$> mkPat Pat.Error <*> pure [] <*> pure os
        loop least lhs rest = \case
          o : os -> do
            (assoc, prec) <- fixityOf o
            if prec < least
              then pure (lhs, rest, o : os)
              else do
                (rhs', rest', os') <- climb (if assoc == RightAssoc then prec else prec + 1) rest os
                k <- constructor o
                joined <- maybe (mkPat Pat.Error) (\k' -> applied o k' before [lhs, rhs']) k
                loop least joined rest' os'
          [] -> pure (lhs, rest, [])
    resolved <- mapM patternOf operands
    (\(q, _, _) -> q) <$> climb 0 resolved operators
  -- The fields not named match anything.
  SPat.Record c fs ->
    constructor c >>= \case
      Nothing -> mkPat Pat.Error
      Just k -> do
        arity <- arityOf k
        given <- placed k [(f, (f, p)) | (f, p) <- fs]
        -- A field with no pattern is the variable of its name.
        let field (f, p) = maybe (binder (unqualified f) >>= mkPat . Pat.Var) patternOf p
        -- And so is each field not named, if they are asked for with two
        -- dots, where a variable was made for it.
        others <- if SPat.recordWild p then ($ c) <$> fieldsOfConstructor else pure []
        scope <- asks envValues
        let other i = case lookup i others of
              Just f | Just x <- Map.lookup f scope -> mkPat (Pat.Var x)
              Just _ -> complain "`..` binds no variables in a pattern at the top of a module" >> mkPat Pat.Wild
              Nothing -> mkPat Pat.Wild
        ps <- forM [0 .. arity - 1] (\i -> maybe (other i) field (lookup i given))
        whole <- mkPat (Pat.Con k ps)
        -- Fields are matched in the order they are written.
        let order = map fst given
        if and (zipWith (<) order (drop 1 order)) then pure whole else mkPat (Pat.Ordered order whole)
  SPat.As x q -> do
    name <- binder x
    patternOf q >>= mkPat . Pat.As name
  SPat.Paren q -> patternOf q
  SPat.Lazy q -> patternOf q >>= mkPat . Pat.Lazy
  SPat.Bang q -> patternOf q >>= mkPat . Pat.Bang
  -- A view pattern is a variable, and a qualifier for whoever has the
  -- pattern to add: that the function applied to it matches. One inside
  -- its pattern is after it, since it is of what that pattern binds.
  SPat.View f q -> expr f >>= (`viewing` patternOf q)
  -- A pattern said to have a type matches what it does of the value
  -- taken as of that type: a view pattern of the identity at the type.
  SPat.Sig q ty -> do
    Signature vs context said <- signatureOf [] ty
    again <- substituted (NameId (-1)) said said
    x <- newName "x" Local
    function <- (Type.Type <$> node <*> pure (Type.Fun said again)) >>= \to -> do
      parameter <- mkPat (Pat.Var x)
      body <- mk (Var x)
      identity <- mk (Lam [parameter] body)
      mk (Annotated identity (Signature vs context to))
    viewing function (patternOf q)
  SPat.Tuple [] -> con "()" []
  SPat.Tuple ps -> con (tupleName (length ps)) ps
  SPat.List ps -> do
    nil <- con "[]" []
    foldM (\rest q -> do q' <- patternOf q; c <- constructor ":"; mkPat (maybe Pat.Error (\k -> Pat.Con k [q', rest]) c)) nil (reverse ps)
  where
    con c ps = do
      found <- constructor c
      ps' <- mapM patternOf ps
      mkPat (maybe Pat.Error (`Pat.Con` ps') found)

literal :: Syntax.Literal -> M (Maybe Literal)
literal = \case
  Syntax.Number n -> pure (Just (Number n))
  Syntax.Character c -> pure (Just (Character c))
  Syntax.Text s -> pure (Just (Text s))
  Syntax.Fraction s -> case readFloat s of
    -- With NumDecimals a number written with an exponent that is whole
    -- is a whole number, of any type that has them.
    [(q, "")] -> do
      whole <- (&& denominator q == 1) <$> enabled NumDecimals
      pure (Just (if whole then Number (numerator q) else Fraction q))
    _ -> Nothing <$ complain ("malformed literal: " ++ s)
  Syntax.Malformed s -> Nothing <$ complain ("malformed literal: " ++ s)

-- Expressions ---------------------------------------------------------------

expr :: Syntax.Expr -> M Expr
expr e = within e $ case e of
  Expr.Var x -> variable x >>= mk
  Expr.Con c -> construct c
  -- With OverloadedStrings a string is of any type that has them.
  Expr.Lit l@(Syntax.Text _) -> do
    overloaded <- enabled OverloadedStrings
    string <- literal l >>= mk . maybe Error Lit
    if overloaded then call "fromString" [string] else pure string
  Expr.Lit l -> literal l >>= mk . maybe Error Lit
  Expr.App f args -> do
    f' <- case Expr.typeArguments e of
      [] -> expr f
      types -> typeApplied f types
    mapM expr args >>= apply f'
  Expr.Infix items -> infixExpr items
  Expr.Lam ps body -> do
    (ps', scope, vs) <- patternsViewed ps
    extend scope (expr body) >>= viewedExpr vs >>= mk . Lam ps'
  Expr.Let ds body -> do
    (bs, strictly, body') <- bindingsForced ds (expr body)
    forcingAll strictly body' >>= mk . Let bs
  Expr.If c t f -> (If <$> expr c <*> expr t <*> expr f) >>= mk
  Expr.Case scrutinee alts -> do
    scrutinee' <- expr scrutinee
    alts' <- forM alts $ \a@(Syntax.Alt p r) -> within a $ do
      n <- node
      (ps, scope, vs) <- patternsViewed [p]
      Alt n (head' ps) . viewed vs <$> extend scope (rhs r)
    mk (Case scrutinee' alts')
  Expr.Do stmts -> doBlock stmts
  -- \case alternatives is \x -> case x of alternatives.
  Expr.LamCase alts -> do
    x <- newName "x" Local
    scrutinee <- mk (Var x)
    parameter <- mkPat (Pat.Var x)
    alts' <- forM alts $ \a@(Syntax.Alt p r) -> within a $ do
      n <- node
      (ps, scope, vs) <- patternsViewed [p]
      Alt n (head' ps) . viewed vs <$> extend scope (rhs r)
    mk (Case scrutinee alts') >>= mk . Lam [parameter]
  -- An if with guards is a case with nothing to take apart, and guards.
  Expr.MultiIf alternatives -> do
    scrutinee <- construct "()"
    n <- node
    anything <- mkPat Pat.Wild
    rhs alternatives >>= mk . Case scrutinee . pure . Alt n anything
  -- A tuple with places left out is a function of them.
  Expr.TupleSection slots -> do
    c <- construct (tupleName (length slots))
    filled <- forM slots $ \case
      Just given -> (,) Nothing <$> expr given
      Nothing -> do
        x <- newName "x" Local
        (,) . Just <$> mkPat (Pat.Var x) <*> mk (Var x)
    apply c (map snd filled) >>= mk . Lam (mapMaybe fst filled)
  Expr.Paren inner -> expr inner
  Expr.Tuple [] -> construct "()"
  Expr.Tuple es -> do
    c <- construct (tupleName (length es))
    mapM expr es >>= apply c
  Expr.List es -> do
    nil <- construct "[]"
    es' <- mapM expr es
    foldM (\rest x -> do c <- construct ":"; apply c [x, rest]) nil (reverse es')
  Expr.Range from next to -> do
    let ends = [from] ++ maybe [] pure next ++ maybe [] pure to
    f <- builtin ("enumFrom" ++ maybe "" (const "Then") next ++ maybe "" (const "To") to)
    mapM expr ends >>= apply f
  Expr.Comprehension element qs -> case Expr.comprehensionBranches e of
    [] -> comprehension (expr element) qs
    more -> parallel element (qs : more)
  -- A record built: the constructor applied, with a field not given
  -- being an error to look at.
  e0@(Expr.Record (Expr.Con c) fs) ->
    constructor c >>= \case
      Nothing -> mk Error
      Just k -> do
        arity <- arityOf k
        named <- placed k [(f, (f, e)) | (f, e) <- fs]
        -- With two dots, each field not named is given the variable of
        -- its name, if there is one that is not the field itself.
        others <- if Expr.recordWild e0 then ($ c) <$> fieldsOfConstructor else pure []
        scope <- asks envValues
        top <- asks envTop
        let given = named ++ [(i, (f, Nothing)) | (i, f) <- others, i `notElem` map fst named, Just x <- [Map.lookup f scope], Just x /= Map.lookup f top]
        musts <- gets (IntMap.findWithDefault [] (nameKey k) . strict)
        when (or [must | (i, must) <- zip [0 ..] musts, i `notElem` map fst given]) $
          complain ("a strict field of " ++ c ++ " is not given")
        arguments <- forM [0 .. arity - 1] (\i -> maybe (mk (Prim MatchFail)) fieldValue (lookup i given))
        constructing k >>= (`apply` arguments)
  -- A record changed: for each constructor that has all the fields, the
  -- same constructor with those fields replaced.
  Expr.Record subject fs -> do
    subject' <- expr subject
    known <- forM fs $ \(f, _) -> do
      field <- asks (Map.lookup f . envTop)
      gets (\st -> maybe [] (\x -> IntMap.findWithDefault [] (nameKey x) (fields st)) field)
    let candidates = case known of
          first : rest -> [c | (c, _) <- first, all (any ((== c) . fst)) rest]
          [] -> []
    when (null candidates) (complain "no constructor has all of these fields")
    alts <- forM candidates $ \c -> do
      arity <- arityOf c
      given <- placed c [(f, (f, e)) | (f, e) <- fs]
      xs <- replicateM arity (newName "x" Local)
      p <- mapM (mkPat . Pat.Var) xs >>= mkPat . Pat.Con c
      arguments <- forM (zip [0 ..] xs) (\(i, x) -> maybe (mk (Var x)) fieldValue (lookup i given))
      body <- mk (Con c) >>= (`apply` arguments)
      Alt <$> node <*> pure p <*> plain body
    mk (Case subject' alts)
  -- Said to have a type with parts left out, it is said nothing of.
  Expr.Typed inner ty -> do
    inner' <- expr inner
    (said, _, partial) <- signatureFull [] ty
    if partial then pure inner' else mk (Annotated inner' said)
  Expr.SectionL operand o -> do
    sectioned True o operand
    f <- operator o
    expr operand >>= apply f . pure
  -- (op e) is \x -> x op e.
  Expr.SectionR o operand -> do
    sectioned False o operand
    x <- newName "x" Local
    f <- operator o
    operand' <- expr operand
    argument <- mk (Var x)
    parameter <- mkPat (Pat.Var x)
    apply f [argument, operand'] >>= mk . Lam [parameter]
  where
    -- A field's value: what is given, or for a field by itself the
    -- variable of its name.
    fieldValue (f, given) = maybe (variable (unqualified f) >>= mk) expr given
    head' ps = case ps of
      p : _ -> p
      [] -> error "Powder.Resolve: a case alternative with no pattern"

-- | A name applied to types: what the first of its type variables are.
-- They are in the order its type has them: a signature's, which for a
-- method is its class's first, and for a constructor its type's
-- parameters. A type left out, as _, leaves its variable to be found.
typeApplied :: Syntax.Expr -> [SType.Type] -> M Expr
typeApplied f types = case f of
  Expr.Paren inner -> typeApplied inner types
  Expr.Var x ->
    variable x >>= \case
      Var n -> mk (Var n) >>= at'
      other -> complain "only a name can be applied to a type" >> mk other
  Expr.Con c ->
    constructor c >>= \case
      Nothing -> mk Error
      Just k -> do
        synonym <- isSynonym k
        if synonym then complain "a pattern synonym cannot be applied to a type" >> construct c else constructingWith at' k
  _ -> complain "only a name can be applied to a type" >> expr f
  where
    at' inner = do
      given <- forM types $ \case
        SType.Wild -> pure Nothing
        t -> Just <$> type' t
      mk (Applied inner given)

-- | A section's operand must be the whole of one side of its operator:
-- every operator in it must bind tighter than the section's, or as
-- tightly and group towards it.
sectioned :: Bool -> Syntax.Operator -> Syntax.Expr -> M ()
sectioned left o operand = case operand of
  Expr.Infix items -> do
    (assoc, prec) <- fixityOf (operatorName o)
    inside <- forM items $ \case
      Expr.Operator o' -> Just <$> fixityOf (operatorName o')
      Expr.Negate -> pure (Just (LeftAssoc, 6))
      Expr.Operand _ -> pure Nothing
    let towards = if left then LeftAssoc else RightAssoc
        fits (assoc', prec') = prec' > prec || (prec' == prec && assoc' == towards && assoc == towards)
    unless (all fits (catMaybes inside)) $
      within o (complain "the operand of this section is not the whole of one side of the operator: put it in parentheses")
  _ -> pure ()

-- | A list comprehension, as the Report says what one means: a condition
-- is a choice between the rest and nothing, and a generator is the rest
-- for each element that matches, all put end to end.
comprehension :: M Expr -> [Stmt.Stmt] -> M Expr
comprehension element = \case
  [] -> do
    x <- element
    nil <- construct "[]"
    c <- construct ":"
    apply c [x, nil]
  s : rest -> within s $ case s of
    Stmt.Expr c -> (If <$> expr c <*> comprehension element rest <*> construct "[]") >>= mk
    Stmt.Let ds -> do
      (bs, strictly, body) <- bindingsForced ds (comprehension element rest)
      forcingAll strictly body >>= mk . Let bs
    Stmt.Bind p from -> do
      from' <- expr from
      (ps, scope, vs) <- patternsViewed [p]
      body <- extend scope (comprehension element rest)
      nil <- construct "[]"
      f <- matchingViewed ps vs body nil
      call "concatMap" [f, from']

-- | A list comprehension whose qualifiers are in several rows, gone
-- through side by side: the element for each of what the rows give
-- together, as far as the shortest goes. Each row is a comprehension of
-- its own, of what it binds, and they are zipped.
parallel :: Syntax.Expr -> [[Stmt.Stmt]] -> M Expr
parallel element rows = do
  wild <- fieldsOfConstructor
  let bound row = nub (concat [case s of Stmt.Bind p _ -> patternNames wild p; Stmt.Let ds -> concatMap (definesWith wild) (grouped ds); Stmt.Expr _ -> [] | s <- row])
      names' = map bound rows
      doubled = concat names' \\ nub (concat names')
  unless (null doubled) (complain (unwords (nub doubled) ++ " is bound in more than one row of this comprehension"))
  lists <- forM (zip rows names') $ \(row, xs) -> comprehension (mapM (variable >=> mk) xs >>= tupleOf) row
  scope <- forM (concat names') (\x -> (,) x <$> newName x Local)
  ps <- forM names' (mapM (\x -> mkPat (Pat.Var (fromMaybe (error "Powder.Resolve: a name of a comprehension was not made") (lookup x scope)))) >=> tuplePatOf)
  zipped <- zipAll lists
  p <- nest ps
  body <- extend scope (expr element)
  f <- mk (Lam [p] body)
  call "map" [f, zipped]
  where
    zipAll = \case
      [l] -> pure l
      l : ls -> zipAll ls >>= \rest -> call "zip" [l, rest]
      [] -> construct "[]"
    nest = \case
      [p] -> pure p
      p : ps -> nest ps >>= \rest -> constructor (tupleName 2) >>= mkPat . maybe Pat.Error (`Pat.Con` [p, rest])
      [] -> mkPat Pat.Wild

-- | A function that is one thing where its argument matches a pattern
-- and another where it does not.
-- Matching is also that some qualifiers hold, which the pattern's view
-- patterns ask for.
matchingViewed :: [Pat] -> [Qualifier] -> Expr -> Expr -> M Expr
matchingViewed ps vs body otherwise' = do
  x <- newName "x" Local
  scrutinee <- mk (Var x)
  wild <- mkPat Pat.Wild
  alts <- sequence ([Alt <$> node <*> pure p <*> (viewed vs <$> plain body) | p <- take 1 ps] ++ [Alt <$> node <*> pure wild <*> plain otherwise'])
  parameter <- mkPat (Pat.Var x)
  mk (Case scrutinee alts) >>= mk . Lam [parameter]

-- | Whether a pattern can fail to match.
failable :: SPat.Pat -> Bool
failable = \case
  SPat.Var _ -> False
  SPat.Wild -> False
  SPat.Lazy _ -> False
  SPat.Bang p -> failable p
  SPat.Sig p _ -> failable p
  SPat.As _ p -> failable p
  SPat.Paren p -> failable p
  SPat.Tuple ps -> any failable ps
  _ -> True

-- | What an operator stands for: a constructor if it is spelled like one.
operator :: Syntax.Operator -> M Expr
operator o = within o $ do
  let name = operatorName o
  if take 1 (unqualified name) == ":" || any isUpper (take 1 (unqualified name))
    then construct name
    else variable name >>= mk

-- | Group a row of operands and operators by precedence and
-- associativity. This is the algorithm of the Report, section 10.6: a
-- prefix minus is an operator of precedence six that groups to the left,
-- and may only follow an operator that binds less tightly than that; two
-- operators of one precedence must both group to the left or both to the
-- right.
infixExpr :: [Expr.Item] -> M Expr
infixExpr items = do
  (e, leftover) <- negated Nothing (NonAssoc, -1) items
  unless (null leftover) (complain "this expression has an operator with nothing after it")
  pure e
  where
    -- An operand, which may be negated, and then what follows it, as far
    -- as the operator before it reaches.
    negated :: Maybe Syntax.Operator -> (Assoc, Int) -> [Expr.Item] -> M (Expr, [Expr.Item])
    negated before fixity = \case
      Expr.Operand e : rest -> expr e >>= \e' -> following before fixity e' rest
      Expr.Negate : rest -> do
        when (snd fixity >= 6) $
          maybe id within before (complain "a minus sign cannot follow an operator that binds as tightly as it does, or tighter: put what it negates in parentheses")
        (r, rest') <- negated Nothing (LeftAssoc, 6) rest
        e <- builtin "negate" >>= (`apply` [r])
        following before fixity e rest'
      rest -> do
        complain "an operand is missing here"
        (,) <$> mk Error <*> pure (drop 1 rest)

    -- What follows an operand: operators that bind tighter than the one
    -- before it are its own.
    following before (assoc1, prec1) lhs = \case
      Expr.Operator o : rest -> do
        (assoc2, prec2) <- fixityOf (operatorName o)
        if prec1 == prec2 && (assoc1 /= assoc2 || assoc1 == NonAssoc)
          then do
            within o (complain ("the operators either side of this do not say how they group: " ++ grouping assoc1 prec1 ++ " before and " ++ grouping assoc2 prec2 ++ " here"))
            (lhs, []) <$ negated (Just o) (assoc2, prec2) rest
          else
            if prec1 > prec2 || (prec1 == prec2 && assoc1 == LeftAssoc)
              then pure (lhs, Expr.Operator o : rest)
              else do
                f <- operator o
                (r, rest') <- negated (Just o) (assoc2, prec2) rest
                joined <- apply f [lhs, r]
                following before (assoc1, prec1) joined rest'
      rest -> pure (lhs, rest)

    grouping assoc prec = (case assoc of LeftAssoc -> "infixl "; RightAssoc -> "infixr "; NonAssoc -> "infix ") ++ show prec

-- | @do@ is sugar for the top-level @>>=@.
doBlock :: [Stmt.Stmt] -> M Expr
doBlock = \case
  [Stmt.Expr e] -> expr e
  Stmt.Expr e : rest@(_ : _) -> do
    action <- expr e
    wild <- mkPat Pat.Wild
    continue action [wild] [] [] rest
  -- Where the pattern can fail to match, the monad says what then.
  Stmt.Bind p e : rest@(_ : _) | failable p -> do
    action <- expr e
    (ps, scope, vs) <- patternsViewed [p]
    body <- extend scope (doBlock rest)
    failed <- mk (Lit (Text "pattern match failure in a do block")) >>= call "fail" . pure
    k <- matchingViewed ps vs body failed
    call ">>=" [action, k]
  Stmt.Bind p e : rest@(_ : _) -> do
    action <- expr e
    (ps, scope, vs) <- patternsViewed [p]
    continue action ps scope vs rest
  Stmt.Let ds : rest@(_ : _) -> do
    (bs, strictly, body) <- bindingsForced ds (doBlock rest)
    forcingAll strictly body >>= mk . Let bs
  _ -> complain "the last statement in a 'do' block must be an expression" >> mk Error
  where
    continue action ps scope vs rest = do
      bind <- builtin ">>="
      k <- extend scope (doBlock rest) >>= viewedExpr vs >>= mk . Lam ps
      apply bind [action, k]

