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
import Data.List (intercalate, isSuffixOf, nub, (\\))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Char (isAlphaNum, isUpper)
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe, mapMaybe)
import qualified Data.Set as Set
import Numeric (readFloat)
import Powder.Core (ConInfo (..), Name, Prim (MatchFail), primName)
import Powder.Extensions (Extension (..), Extensions)
import Powder.Diagnostics (Problem (..))
import Powder.HIR hiding (mine)
import qualified Powder.HIR.Pat as Pat
import qualified Powder.HIR.Type as Type
import Powder.Parser (Parsed (..), parseModule)
import Powder.Syntax.AST (Ast, Module, cast, moduleDecls, moduleHeader, operatorName, syntax)
import qualified Powder.Syntax.AST as Syntax
import Powder.Syntax.AST.Decl (Assoc (..))
import qualified Powder.Syntax.AST.Decl as Decl
import qualified Powder.Syntax.AST.Expr as Expr
import qualified Powder.Syntax.AST.Pat as SPat
import qualified Powder.Syntax.AST.Stmt as Stmt
import qualified Powder.Syntax.AST.Type as SType
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
    -- | The extensions each file is written with.
    envLanguages :: Map.Map FilePath Extensions,
    -- | Whether what is being resolved was made up here, so that it is
    -- all placed where what it was made up for is.
    envPinned :: Bool
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
    problems :: [Problem]
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
    linkStrict :: Table [Bool]
  }

instance Semigroup Linkage where
  Linkage a b c d e f g <> Linkage a' b' c' d' e' f' g' =
    Linkage (Map.union a a') (b <|> b') (IntMap.union c c') (d <|> d') (IntMap.union e e') (IntMap.union f f') (IntMap.union g g')

instance Monoid Linkage where
  mempty = Linkage Map.empty Nothing IntMap.empty Nothing IntMap.empty IntMap.empty IntMap.empty

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
    seedImports :: [(String, [String])]
  }

-- | Resolve modules as 'resolve' does, as a package compiled after
-- others if a seed is given. Then the program has the others' types,
-- classes and instances in it beside its own, but only its own
-- definitions; no module's names are spelled plainly; and a module of the
-- others can be imported. With the program comes what a package compiled
-- after this one will need to be given in its turn.
resolveAfter :: Maybe Seed -> Map.Map FilePath Extensions -> [Name] -> [(FilePath, Name, Module)] -> Maybe (Name, Maybe Name, FilePath, Syntax.Expr) -> Either [Problem] (Program, Linkage)
resolveAfter seed languages lenient sources extra = case reverse (problems final) of
  [] -> Right (program, linked {linkFields = fields final, linkFixities = fixityTable final, linkStrict = strict final})
  found -> Left found
  where
    program = Program (bindings ++ reverse (generated final)) (names final) (origins final) (signatures final) (reverse (dataTypes final)) (synonyms final) (classes final) (reverse (instances final)) builtins spelled (defaults final) (newtypes final) (maybe [] seedModules seed ++ [(file, n) | (file, n, _) <- sources] ++ [(file, root) | Just (_, _, file, _) <- [extra]]) (maybe [] seedImports seed ++ [(n, [name | (name, _, _, _) <- importsOf n]) | n <- moduleNames]) (maybe (-1) seedFloor seed)
    ((linked, (bindings, builtins)), final) =
      runState
        (runReaderT run (Env Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty "" Nothing "" spelled Set.empty Map.empty Map.empty Map.empty languages False))
        initial
    initial = case seed of
      Nothing -> St 0 IntMap.empty IntMap.empty IntMap.empty IntMap.empty [] IntMap.empty [] IntMap.empty [] [] IntMap.empty [] IntMap.empty []
      Just s ->
        St
          (seedFloor s)
          (seedNames s)
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
      [(name, qualified, alias, items) | (_, m) <- sourcesOf n, Decl.Import name qualified alias items <- moduleDecls m, name /= n]
        ++ [("Prelude", False, Nothing, Nothing) | hasPrelude, n /= "Prelude", null [() | (_, m) <- sourcesOf n, Decl.Import "Prelude" _ _ _ <- moduleDecls m]]

    run = do
      (wiredTypes, wiredConstructors, wiredBuiltins) <- maybe wiredIn pure (linkWired before)
      -- Every name a module declares at its top, before what any of them
      -- is: types and their constructors, classes, and values.
      declaredTypes <- forM sources $ \(file, n, m) -> inPart n file . fmap concat . forM (moduleDecls m) $ \d -> within d $ case d of
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
      let parts = zip3 sources declaredTypes declaredClasses
          partsOf n = [(file, m, ts, cs) | ((file, n', m), ts, cs) <- parts, n' == n]
          methodsOf n = Map.fromList [(x, c) | (_, _, _, cs) <- partsOf n, (_, c, xs) <- cs, x <- xs]
          definedBy n =
            nub
              ( Map.keys (methodsOf n)
                  ++ concat [concatMap defines (grouped (moduleDecls m)) | (_, m) <- sourcesOf n]
                  ++ concat [fieldsOf d | (_, m) <- sourcesOf n, d <- moduleDecls m]
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
                scopeConstructors = Map.fromList [(c, [x]) | (_, _, ts, _) <- partsOf n, (_, _, cs, _) <- ts, (c, x) <- cs],
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
                  envGlobals = one (scopeValues (basis n)),
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
        let find o = Map.lookup o (envOwnValues env) <|> Map.lookup o (envOwnConstructors env) <|> Map.lookup o (envValues env) <|> Map.lookup o (envConstructors env)
        declareFixities find (moduleDecls m ++ concat [ds | Decl.Class _ ds <- moduleDecls m])
      -- Types first, since everything else may mention them; then classes,
      -- before the instances that need to know their methods.
      forM_ parts $ \((file, n, m), ts, _) ->
        inModule n file (mapM_ (typeDecl (Map.fromList [(t, (x, map snd cs)) | (t, x, cs, _) <- ts])) (moduleDecls m))
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
          supers c = maybe [] (map nameKey . classSupers) (IntMap.lookup c (classes known))
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
        deriving' (Map.fromList [(t, x) | (t, x, _, _) <- ts]) d
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
          _ -> pure []
        (,) [globalsOf n Map.! f | Equations _ f _ <- gs] . (++ foreigns) <$> mapM binding gs
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
      pure (Linkage exports (if "Powder.Base" `elem` moduleNames then Just (own "Powder.Base") else linkBase before) belongings (Just (wiredTypes, wiredConstructors, wiredBuiltins)) IntMap.empty IntMap.empty IntMap.empty, (kept ++ asked, wiredBuiltins {builtinNumeric = method "Num" "fromInteger", builtinEquality = method "Eq" "==", builtinFractional = (\(c, x) k -> (c, x, k)) <$> method "Fractional" "fromRational" <*> Map.lookup ":%" (one (scopeConstructors (basis root)))}))

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
declareFixities :: (Name -> Maybe NameId) -> [Syntax.Decl] -> M ()
declareFixities find ds = do
  let given = [(o, d) | d@(Decl.Fixity _ _ os) <- ds, o <- os]
  forM_ (nub (map fst given \\ nub (map fst given))) $ \o ->
    forM_ (take 1 (reverse [d | (o', d) <- given, o' == o])) (\d -> within d (complain ("a fixity is given for " ++ o ++ " more than once")))
  forM_ ds $ \d -> case d of
    Decl.Fixity assoc prec os -> do
      when (prec < 0 || prec > 9) (within d (complain "a precedence is from 0 to 9"))
      forM_ os $ \o -> case find o of
        Just x -> modify (\st -> st {fixityTable = IntMap.insert (nameKey x) (assoc, prec) (fixityTable st)})
        Nothing -> within d (complain ("a fixity is given for " ++ o ++ ", which is not defined here"))
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
construct name = constructor name >>= mk . maybe Error Con

-- Types ----------------------------------------------------------------------

-- | How many fields a constructor has.
arityOf :: NameId -> M Int
arityOf c = gets $ \st -> case IntMap.lookup (nameKey c) (names st) of
  Just (NameInfo _ (Constructor info _) _) -> conArity info
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
  field <- asks (Map.lookup f . envTop)
  places <- gets (\st -> maybe [] (\x -> IntMap.findWithDefault [] (nameKey x) (fields st)) field)
  case lookup c places of
    Just i -> pure (Just (i, a))
    Nothing -> Nothing <$ complain (f ++ " is not a field of this constructor")

-- | The types and constructors that are there before any are declared:
-- those of literals and of what syntax builds, as the names they go by,
-- with the ones the language itself refers to.
wiredIn :: M ([(Name, NameId)], [(Name, NameId)], Builtins)
wiredIn = do
  integer <- newName "Integer" TypeConstructor
  int <- newName "Int" TypeConstructor
  -- The arrow has an identity fixed beforehand, which the checker knows.
  modify (\st -> st {names = IntMap.insert (-1) (NameInfo "->" TypeConstructor "") (names st)})
  argument <- newName "a" TypeVariable
  result <- newName "b" TypeVariable
  addDataType (DataType (NameId (-1)) [argument, result] [])
  double <- newName "Double" TypeConstructor
  float <- newName "Float" TypeConstructor
  char <- newName "Char" TypeConstructor
  io <- newName "IO" TypeConstructor
  a <- newName "a" TypeVariable
  mapM_ (\(t, ps) -> addDataType (DataType t ps [])) [(integer, []), (int, []), (double, []), (float, []), (char, []), (io, [a])]
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
  zip (map fst cs) ids <$ constructorsAs ids t parameters cs

-- | Say what the constructors of a type are, under names they have been
-- given already. In what is compiled a constructor goes by its name, with
-- its module's before it unless that is the root.
constructorsAs :: [NameId] -> NameId -> [NameId] -> [(Name, [Type.Type])] -> M ()
constructorsAs ids t parameters cs = do
  home <- asks envModule
  plainly <- asks ((|| null home) . (== home) . envRoot)
  forM_ (zip3 [0 ..] cs ids) $ \(tag, (c, fields), NameId n) -> do
    let spelling = if plainly then c else home ++ "." ++ c
    modify (\st -> st {names = IntMap.insert n (NameInfo c (Constructor (ConInfo spelling tag (length fields) (length cs)) ids) home) (names st)})
  addDataType (DataType t parameters (zip ids (map snd cs)))

-- | What a declaration of a type says it is, given the names this module's
-- types were given. Gives the constructors it declares.
typeDecl :: Map.Map Name (NameId, [NameId]) -> Syntax.Decl -> M ()
typeDecl own d = within d $ case d of
  Decl.Data t parameters cs -> do
    scope <- typeParameters parameters
    declared <- withTypeVariables scope . forM cs $ \c@(Syntax.Constructor name tys) -> within c ((,) name <$> mapM type' tys)
    let (name, ids) = own Map.! t
    constructorsAs ids name (map snd scope) declared
    modify (\st -> st {strict = IntMap.union (IntMap.fromList (zip (map nameKey ids) (map Syntax.constructorStrict cs))) (strict st)})
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
    body <- withTypeVariables scope (type' ty)
    modify (\st -> st {synonyms = IntMap.insert (nameKey (fst (own Map.! t))) (Synonym (map snd scope) body) (synonyms st)})
  _ -> pure ()
  where
    typeParameters parameters = do
      forM_ (nub (parameters \\ nub parameters)) (\x -> complain ("the type variable " ++ x ++ " is given twice"))
      forM (nub parameters) (\x -> (,) x <$> newName x TypeVariable)

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
  where
    mkType form = (`Type.Type` form) <$> node
    applyType = foldM (\f a -> mkType (Type.App f a))
    typeConstructor c =
      asks (Map.lookup c . envTypes) >>= \case
        Just n -> mkType (Type.Con n)
        Nothing -> missing "unknown type " c >> mkType Type.Error

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
    declared <- signatureOf given ty
    forM_ xs $ \x ->
      if x `notElem` defined
        then complain ("the type signature for " ++ x ++ " has no definition to go with it")
        else do
          NameId n <- binder x
          modify (\st -> st {signatures = IntMap.insert n declared (signatures st)})
  _ -> pure ()

-- | A type as a signature gives it, with its context if it has one, over
-- the variables it is given and then its own.
signatureOf :: [(Name, NameId)] -> SType.Type -> M Signature
signatureOf given ty = do
  let (assertions, body) = case ty of
        SType.Qualified context inner -> (assertionsOf context, inner)
        _ -> ([], ty)
  own <- forM (nub (typeVariables ty) \\ map fst given) (\x -> (,) x <$> newName x TypeVariable)
  let scope = given ++ own
  (constrained, resolved) <- withTypeVariables scope ((,) . catMaybes <$> mapM constraint assertions <*> type' body)
  pure (Signature (map snd scope) constrained resolved)

-- Classes -------------------------------------------------------------------

-- | The head of a class or an instance taken apart: the assertions of its
-- context, the class, and what the class is applied to.
classHead :: SType.Type -> Maybe ([SType.Type], Name, SType.Type)
classHead = \case
  SType.Qualified context inner -> (\(_, c, t) -> (assertionsOf context, c, t)) <$> classHead inner
  SType.App (SType.Con c) [t] -> Just ([], c, t)
  _ -> Nothing

-- | The assertions of a context: one, or several in parentheses.
assertionsOf :: SType.Type -> [SType.Type]
assertionsOf = \case
  SType.Tuple ts -> ts
  SType.Paren t -> [t]
  t -> [t]

-- | Resolve one assertion of a context, whose type variables are in scope.
constraint :: SType.Type -> M (Maybe Constraint)
constraint t = within t $ case t of
  SType.App (SType.Con c) [argument] ->
    asks (Map.lookup c . envClasses) >>= \case
      Just k -> Just . Constraint k <$> type' argument
      Nothing -> Nothing <$ complain ("unknown class " ++ c)
  _ -> Nothing <$ complain "a context is made of classes, each applied to a type"

-- | What a class declaration says the class is, given the names this
-- module's classes were given. Its methods are in scope already.
classDecl :: Map.Map Name NameId -> Syntax.Decl -> M ()
classDecl own d = within d $ case d of
  Decl.Class (classHead -> Just (context, c, SType.Var a)) ds -> do
    v <- newName a TypeVariable
    supers <- withTypeVariables [(a, v)] (catMaybes <$> mapM constraint context)
    let methods = [x | Decl.Signature xs _ <- ds, x <- xs]
        groups = grouped ds
    -- A method's type is about the class's variable, and may not ask
    -- more of it than the class does.
    forM_ ds $ \m -> case m of
      Decl.Signature _ ty -> within m $ do
        let (asked, body) = case ty of
              SType.Qualified given inner -> (assertionsOf given, inner)
              _ -> ([], ty)
        unless (a `elem` typeVariables body) (complain ("the type of a method must mention the class's variable, " ++ a))
        forM_ asked $ \case
          SType.App _ [SType.Var x] | x == a -> complain ("a method's context cannot ask more of the class's variable, " ++ a)
          _ -> pure ()
      _ -> pure ()
    methodIds <- mapM binder (nub methods)
    declaredTypesOf [(a, v)] methods ds
    defaults <- fmap concat . forM groups $ \g -> case g of
      Equations _ f _ | f `elem` methods -> pure <$> binding g
      _ -> [] <$ within (declOf g) (complain "a class can only define its own methods")
    let cls = Class (own Map.! c) v (map constraintClass supers) methodIds defaults
    modify (\st -> st {classes = IntMap.insert (nameKey (className cls)) cls (classes st)})
  Decl.Class _ _ -> complain "a class is declared as its name and one type variable"
  _ -> pure ()

-- | Note an instance.
addInstance :: Instance -> M ()
addInstance inst = do
  clash <- gets (any (\other -> instanceClass other == instanceClass inst && instanceType other == instanceType inst) . instances)
  when clash (complain "there is already an instance of this class for this type")
  modify (\st -> st {instances = inst : instances st})

-- | What an instance declaration says. The classes are all known.
instanceDecl :: Syntax.Decl -> M ()
instanceDecl d = within d $ case d of
  Decl.Instance (classHead -> Just (context, c, t)) ds ->
    asks (Map.lookup c . envClasses) >>= \case
      Nothing -> complain ("unknown class " ++ c)
      Just k -> do
        forM_ ds $ \s -> case s of
          Decl.Signature _ _ -> within s (complain "an instance cannot give its methods type signatures: the class gives them")
          _ -> pure ()
        scope <- forM (nub (typeVariables t)) (\x -> (,) x <$> newName x TypeVariable)
        (t', context') <- withTypeVariables scope ((,) <$> type' t <*> (catMaybes <$> mapM constraint context))
        cls <- gets (IntMap.lookup (nameKey k) . classes)
        let methods = maybe [] classMethods cls
            (hd, arguments) = typeSpine t' []
            variables = [v | Type.Var v <- map Type.typeForm arguments]
        synonym <- case hd of
          Type.Con tc -> gets (IntMap.member (nameKey tc) . synonyms)
          _ -> pure False
        n <- node
        bs <- fmap concat . forM (grouped ds) $ \g -> case g of
          Equations _ f _ -> do
            -- A method is the class's, whatever else goes by its name.
            spelled <- gets (\st -> [m | m <- methods, fmap nameText (IntMap.lookup (nameKey m) (names st)) == Just (unqualified f)])
            let found = listToMaybe spelled
            -- It must be in scope, though under any name.
            visible <- asks (\env -> maybe False (`elem` Map.elems (envValues env)) found)
            if visible
              then pure <$> bindingAs found g
              else [] <$ within (declOf g) (complain (f ++ " is not a method of the class " ++ c))
          Destructuring {} -> [] <$ within (declOf g) (complain "an instance can only define methods")
        case hd of
          Type.Con tc
            | not synonym && length variables == length arguments && nub variables == variables ->
                addInstance (Instance n k tc variables context' bs)
          Type.Error -> pure ()
          _ -> complain "an instance is for a type constructor applied to type variables, each different"
  Decl.Instance _ _ -> complain "an instance is declared as a class applied to a type"
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

-- | The instances a data declaration asks to have derived, given the
-- names this module's types were given.
deriving' :: Map.Map Name NameId -> Syntax.Decl -> M ()
deriving' own d = within d $ case d of
  Decl.Data t parameters cs -> forM_ (Decl.derived d) $ \c -> do
    -- Whether a field's type is the type being declared, as it is declared.
    let itself = \case
          SType.Paren inner -> itself inner
          SType.Con n -> null parameters && n == t
          SType.App (SType.Con n) arguments -> n == t && map Just parameters == map (\case SType.Var v -> Just v; _ -> Nothing) arguments
          _ -> False
    asks (Map.lookup c . envClasses) >>= \case
      Nothing -> complain ("unknown class " ++ c)
      Just k -> do
        constructors <- fmap catMaybes . forM cs $ \(Syntax.Constructor name fields) ->
          asks (fmap (name,,length fields) . Map.lookup name . envConstructors)
        -- Only a parameter that a field mentions is asked to be an instance.
        let mentioned = concat [concatMap typeVariables tys | Syntax.Constructor _ tys <- cs]
            constrained = filter (`elem` mentioned) (nub parameters)
        allowed <- enabled EmptyDataDeriving
        methods <- case unqualified c of
          -- A type with no constructors has no values to tell apart, to
          -- show or to read.
          _ | null cs, not allowed -> Nothing <$ complain "an instance can be derived for a type with no constructors only with EmptyDataDeriving"
          "Eq" | null cs -> Nothing <$ written (instanceHead c t (nub parameters) constrained ++ "{ _ == _ = True }")
          "Ord" | null cs -> Nothing <$ written (instanceHead c t (nub parameters) constrained ++ "{ compare _ _ = EQ }")
          "Show" | null cs -> Nothing <$ written (instanceHead c t (nub parameters) constrained ++ "{ showsPrec _ v = seq v id }")
          "Read" | null cs -> Nothing <$ written (instanceHead c t (nub parameters) constrained ++ "{ readsPrec _ _ = [] }")
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
          context <- forM [v | (p, v) <- zip (nub parameters) variables, p `elem` constrained] (\v -> Constraint k . (`Type.Type` Type.Var v) <$> node)
          n <- node
          addInstance (Instance n k (own Map.! t) variables context bs)
  _ -> pure ()

-- | Take an instance written out here as if the source had it. What it
-- refers to is the prelude's, whatever the module has of the same names,
-- and anything wrong with it is placed at what it was written for.
written :: String -> M ()
written source = case (parsedProblems parsed, cast (Red.root (parsedTree parsed)) :: Maybe Module) of
  ([], Just m) -> local (\env -> env {envPinned = True, envValues = Map.union (envGlobals env) (envValues env)}) (mapM_ instanceDecl (moduleDecls m))
  _ -> complain "an instance derived here could not be written, which is a fault in the compiler"
  where
    parsed = parseModule source

-- | An instance's head and context, each parameter of the type being
-- asked to be an instance too.
instanceHead :: Name -> Name -> [Name] -> [Name] -> String
instanceHead cls t parameters constrained = "instance " ++ context ++ cls ++ " (" ++ unwords (t : parameters) ++ ") where "
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
  instanceHead cls t parameters constrained
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
ixInstance cls t parameters constrained constructors = instanceHead cls t parameters constrained ++ "{ " ++ intercalate "; " methods ++ " }"
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
    sameFunction (Left (_, f, _, _)) (Left (_, g, _, _)) = f == g
    sameFunction _ _ = False
    gather group = case NE.head group of
      Left (d, f, _, _) -> Equations d f [(d', ps, r) | Left (d', _, ps, r) <- NE.toList group]
      Right (d, p, r) -> Destructuring d p r

-- | The names a group brings into scope.
defines :: Group -> [Name]
defines = \case
  Equations _ f _ -> [f]
  Destructuring _ p _ -> patternNames p

-- | The variables a pattern binds, as they are spelled.
patternNames :: SPat.Pat -> [Name]
patternNames = \case
  SPat.Var x -> [x]
  SPat.As x p -> x : patternNames p
  SPat.Con _ ps -> concatMap patternNames ps
  SPat.Infix a _ b -> patternNames a ++ patternNames b
  SPat.Record _ fs -> concat [maybe [unqualified f] patternNames p | (f, p) <- fs]
  SPat.Bang p -> patternNames p
  SPat.Paren p -> patternNames p
  SPat.Lazy p -> patternNames p
  SPat.Tuple ps -> concatMap patternNames ps
  SPat.List ps -> concatMap patternNames ps
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
-- in their scope.
bindings' :: [Syntax.Decl] -> M a -> M ([Binding], a)
bindings' ds inner = do
  let groups = grouped ds
      defined = concatMap defines groups
  conflicts defined groups
  scope <- forM (nub defined) (\x -> (,) x <$> newName x Local)
  local (\env -> env {envValues = Map.union (Map.fromList scope) (envValues env)}) $ do
    declareFixities (`lookup` scope) ds
    declaredTypesOf [] defined ds
    (,) <$> mapM binding groups <*> inner

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
    Binding n . Function name <$> forM clauses (\(d', ps, r) -> within d' (clause ps r))
  Destructuring d p r -> within d $ do
    -- The names are in scope already: the pattern refers to them.
    n <- node
    p' <- patternOf p
    Binding n . Destructure p' <$> rhs r
  where
    clause ps r = do
      c <- node
      (ps', scope) <- patterns ps
      Clause c ps' <$> extend scope (rhs r)

extend :: [(Name, NameId)] -> M a -> M a
extend scope = local (\env -> env {envValues = Map.union (Map.fromList scope) (envValues env)})

-- | Guards and bodies, in scope of the @where@ that follows them.
rhs :: Syntax.Rhs -> M Rhs
rhs r@(Syntax.Rhs alternatives wheres) = within r $ do
  n <- node
  (bs, guardeds) <- bindings' wheres . forM alternatives $ \(guard', body) -> within body $ do
    g <- node
    uncurry (Guarded g) <$> qualifiers guard' (expr body)
  pure (Rhs n guardeds bs)

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
    (ps, scope) <- patterns [p]
    (qs, a) <- extend scope (qualifiers rest inner)
    pure ([Match q e' | q <- take 1 ps] ++ qs, a)
  Stmt.Let ds -> do
    (bs, (qs, a)) <- bindings' ds (qualifiers rest inner)
    pure (Bindings bs : qs, a)

-- Patterns ------------------------------------------------------------------

-- | Resolve patterns that bind new names, giving the names with them. A
-- name may be bound only once among them.
patterns :: [SPat.Pat] -> M ([Pat], [(Name, NameId)])
patterns ps = do
  let spelled = concatMap patternNames ps
  forM_ (nub (spelled \\ nub spelled)) $ \x ->
    forM_ (take 1 ps) (\p -> within p (complain (x ++ " is bound more than once in these patterns")))
  scope <- forM (nub spelled) (\x -> (,) x <$> newName x Local)
  ps' <- extend scope (mapM patternOf ps)
  pure (ps', scope)

-- | Resolve a pattern whose variables are in scope already, as the names
-- it binds.
patternOf :: SPat.Pat -> M Pat
patternOf p = within p $ case p of
  SPat.Var x -> binder x >>= mkPat . Pat.Var
  SPat.Wild -> mkPat Pat.Wild
  SPat.Lit l -> literal l >>= mkPat . maybe Pat.Error Pat.Lit
  SPat.NegLit (Syntax.Number n) -> mkPat (Pat.Lit (Number (negate n)))
  SPat.NegLit fraction@(Syntax.Fraction _) -> literal fraction >>= mkPat . maybe Pat.Error (\case Fraction q -> Pat.Lit (Fraction (negate q)); l -> Pat.Lit l)
  SPat.NegLit _ -> complain "only a number can be negated in a pattern" >> mkPat Pat.Error
  SPat.Con c ps -> con c ps
  -- A row of patterns with constructors between them groups as the
  -- constructors' fixities say.
  SPat.Infix {} -> do
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
                joined <- mkPat (maybe Pat.Error (`Pat.Con` [lhs, rhs']) k)
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
        ps <- forM [0 .. arity - 1] (\i -> maybe (mkPat Pat.Wild) field (lookup i given))
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
    [(q, "")] -> pure (Just (Fraction q))
    _ -> Nothing <$ complain ("malformed literal: " ++ s)
  Syntax.Malformed s -> Nothing <$ complain ("malformed literal: " ++ s)

-- Expressions ---------------------------------------------------------------

expr :: Syntax.Expr -> M Expr
expr e = within e $ case e of
  Expr.Var x -> variable x >>= mk
  Expr.Con c -> construct c
  Expr.Lit l -> literal l >>= mk . maybe Error Lit
  Expr.App f args -> do
    f' <- expr f
    mapM expr args >>= apply f'
  Expr.Infix items -> infixExpr items
  Expr.Lam ps body -> do
    (ps', scope) <- patterns ps
    extend scope (expr body) >>= mk . Lam ps'
  Expr.Let ds body -> do
    (bs, body') <- bindings' ds (expr body)
    mk (Let bs body')
  Expr.If c t f -> (If <$> expr c <*> expr t <*> expr f) >>= mk
  Expr.Case scrutinee alts -> do
    scrutinee' <- expr scrutinee
    alts' <- forM alts $ \a@(Syntax.Alt p r) -> within a $ do
      n <- node
      (ps, scope) <- patterns [p]
      Alt n (head' ps) <$> extend scope (rhs r)
    mk (Case scrutinee' alts')
  Expr.Do stmts -> doBlock stmts
  -- \case alternatives is \x -> case x of alternatives.
  Expr.LamCase alts -> do
    x <- newName "x" Local
    scrutinee <- mk (Var x)
    parameter <- mkPat (Pat.Var x)
    alts' <- forM alts $ \a@(Syntax.Alt p r) -> within a $ do
      n <- node
      (ps, scope) <- patterns [p]
      Alt n (head' ps) <$> extend scope (rhs r)
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
  Expr.Comprehension element qs -> comprehension element qs
  -- A record built: the constructor applied, with a field not given
  -- being an error to look at.
  Expr.Record (Expr.Con c) fs ->
    constructor c >>= \case
      Nothing -> mk Error
      Just k -> do
        arity <- arityOf k
        given <- placed k [(f, (f, e)) | (f, e) <- fs]
        musts <- gets (IntMap.findWithDefault [] (nameKey k) . strict)
        when (or [must | (i, must) <- zip [0 ..] musts, i `notElem` map fst given]) $
          complain ("a strict field of " ++ c ++ " is not given")
        arguments <- forM [0 .. arity - 1] (\i -> maybe (mk (Prim MatchFail)) fieldValue (lookup i given))
        mk (Con k) >>= (`apply` arguments)
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
  Expr.Typed inner ty -> (Annotated <$> expr inner <*> signatureOf [] ty) >>= mk
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
comprehension :: Syntax.Expr -> [Stmt.Stmt] -> M Expr
comprehension element = \case
  [] -> do
    x <- expr element
    nil <- construct "[]"
    c <- construct ":"
    apply c [x, nil]
  s : rest -> within s $ case s of
    Stmt.Expr c -> (If <$> expr c <*> comprehension element rest <*> construct "[]") >>= mk
    Stmt.Let ds -> do
      (bs, body) <- bindings' ds (comprehension element rest)
      mk (Let bs body)
    Stmt.Bind p from -> do
      from' <- expr from
      (ps, scope) <- patterns [p]
      body <- extend scope (comprehension element rest)
      nil <- construct "[]"
      f <- matching ps body nil
      call "concatMap" [f, from']

-- | A function that is one thing where its argument matches a pattern
-- and another where it does not.
matching :: [Pat] -> Expr -> Expr -> M Expr
matching ps body otherwise' = do
  x <- newName "x" Local
  scrutinee <- mk (Var x)
  wild <- mkPat Pat.Wild
  alts <- sequence ([Alt <$> node <*> pure p <*> plain body | p <- take 1 ps] ++ [Alt <$> node <*> pure wild <*> plain otherwise'])
  parameter <- mkPat (Pat.Var x)
  mk (Case scrutinee alts) >>= mk . Lam [parameter]

-- | Whether a pattern can fail to match.
failable :: SPat.Pat -> Bool
failable = \case
  SPat.Var _ -> False
  SPat.Wild -> False
  SPat.Lazy _ -> False
  SPat.Bang p -> failable p
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
    continue action [wild] [] rest
  -- Where the pattern can fail to match, the monad says what then.
  Stmt.Bind p e : rest@(_ : _) | failable p -> do
    action <- expr e
    (ps, scope) <- patterns [p]
    body <- extend scope (doBlock rest)
    failed <- mk (Lit (Text "pattern match failure in a do block")) >>= call "fail" . pure
    k <- matching ps body failed
    call ">>=" [action, k]
  Stmt.Bind p e : rest@(_ : _) -> do
    action <- expr e
    (ps, scope) <- patterns [p]
    continue action ps scope rest
  Stmt.Let ds : rest@(_ : _) -> do
    (bs, body) <- bindings' ds (doBlock rest)
    mk (Let bs body)
  _ -> complain "the last statement in a 'do' block must be an expression" >> mk Error
  where
    continue action ps scope rest = do
      bind <- builtin ">>="
      k <- extend scope (doBlock rest) >>= mk . Lam ps
      apply bind [action, k]

