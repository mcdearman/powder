{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | The forms of a declaration. Import qualified.
module Powder.Syntax.AST.Decl
  ( Decl (Data, Clause, PatBind, Signature, Fixity, TypeSynonym, Class, Instance, Import, Default, Foreign, ForeignExport, PatternSynonym, PatternSignature, StandaloneDeriving, KindSignature, RoleAnnotation),
    Assoc (..),
    Listed (..),
    Subordinates (..),
    derived,
    isNewtype,
    patternBuilder,
    kinds,
    dependencies,
    overlap,
    resultKind,
  )
where

import qualified Data.Text as Text
import Powder.Syntax.AST.Internal
import qualified Powder.Syntax.Kind as K
import qualified Powder.Syntax.Red as Red
import Text.Read (readMaybe)

data Assoc = LeftAssoc | RightAssoc | NonAssoc
  deriving (Eq, Show)

kindIs :: [K.SyntaxKind] -> Decl -> Maybe Red.Red
kindIs kinds (syntax -> r) = if Red.kind r `elem` kinds then Just r else Nothing

-- | A @data@ or @newtype@ declaration: the type's name, its parameters and
-- its constructors.
pattern Data :: Name -> [Name] -> [Constructor] -> Decl
pattern Data name parameters constructors <- (kindIs [K.DataDecl] -> Just (dataDecl -> (name, parameters, constructors)))

dataDecl :: Red.Red -> (Name, [Name], [Constructor])
dataDecl r = (typeNameOf r, parametersOf r, children r)

-- | The name of the type a declaration is of: the first capitalised name
-- or operator of its head, which need not come first.
typeNameOf :: Red.Red -> Name
typeNameOf r = case [x | (k, x) <- takeWhile ((`notElem` [K.Equals, K.KwWhere, K.KwDeriving]) . fst) (spelled r), k `elem` [K.ConId, K.ConSym, K.VarSym]] of
  x : _ -> x
  [] -> nameOf r

-- | The kind a data declaration is said to have after its variables, if
-- it is: what the type is when it is applied to those.
resultKind :: Decl -> Maybe Kind
resultKind = kindOf . syntax

-- | The kinds a declaration gives the type variables its head names, for
-- those it does.
kinds :: Decl -> [(Name, Kind)]
kinds = kindedBinders . syntax

-- | Whether a declaration of a type is a @newtype@'s.
isNewtype :: Decl -> Bool
isNewtype (syntax -> r) = K.KwNewtype `elem` map Red.kind (Red.tokens r)

-- | The classes a data declaration asks to have instances derived for,
-- each with how, if that is said: @stock@, @newtype@ or @anyclass@. It
-- is said after the word deriving, of which there may be several.
derived :: Decl -> [(Maybe Name, Name)]
derived (syntax -> r) = concat [clauses Nothing (spelled d) | d <- Red.nodes r, Red.kind d == K.Deriving]
  where
    clauses how = \case
      (K.KwDeriving, _) : (K.KwNewtype, _) : rest -> clauses (Just "newtype") rest
      (K.KwDeriving, _) : (K.VarId, s) : rest | s `elem` ["stock", "anyclass"] -> clauses (Just s) rest
      (K.KwDeriving, _) : rest -> clauses Nothing rest
      (k, c) : rest | k `elem` [K.ConId, K.QConId] -> (how, c) : clauses how rest
      _ : rest -> clauses how rest
      [] -> []

-- | An instance derived apart from its type's declaration: how, if that
-- is said, and the instance's head, with perhaps a context.
pattern StandaloneDeriving :: Maybe Name -> Type -> Decl
pattern StandaloneDeriving how hd <- (kindIs [K.StandaloneDerivingDecl] -> Just (standalone -> (how, hd)))

standalone :: Red.Red -> (Maybe Name, Type)
standalone r = (how, child 0 r)
  where
    how = case drop 1 (spelled r) of
      (K.KwNewtype, _) : _ -> Just "newtype"
      (K.VarId, s) : _ -> Just s
      _ -> Nothing

-- | The type variables a declaration's head names.
parametersOf :: Red.Red -> [Name]
parametersOf r = concat [if Red.kind c == K.VarId then [Text.unpack (Red.text c)] else [nameOf c | Red.kind c == K.KindedBinder] | c <- Red.children r]

-- | A class: its head, which is the class applied to its variable, with
-- perhaps a context before it; and the declarations of its methods.
pattern Class :: Type -> [Decl] -> Decl
pattern Class hd methods <- (kindIs [K.ClassDecl] -> Just (classDecl -> (hd, methods)))

-- | An instance: its head, which is the class applied to a type, with
-- perhaps a context before it; and the definitions of its methods.
pattern Instance :: Type -> [Decl] -> Decl
pattern Instance hd methods <- (kindIs [K.InstanceDecl] -> Just (classDecl -> (hd, methods)))

-- | A class's functional dependencies: for each, the variables that
-- decide, and the ones they decide.
dependencies :: Decl -> [([Name], [Name])]
dependencies (syntax -> r) = concat [parts (drop 1 (spelled f)) | f <- Red.nodes r, Red.kind f == K.FunDeps]
  where
    parts ts = case break ((== K.Comma) . fst) ts of
      ([], []) -> []
      (one, rest) ->
        let (from, to) = break ((== K.RightArrow) . fst) one
         in ([x | (K.VarId, x) <- from], [x | (K.VarId, x) <- to]) : parts (drop 1 rest)

-- | What an instance says of other instances that fit the same types, in
-- a pragma after the word instance: OVERLAPPING, OVERLAPPABLE or
-- OVERLAPS.
overlap :: Decl -> Maybe Name
overlap (syntax -> r) = case [w | t <- Red.descendants r, Red.kind t == K.Pragma, w <- words (map (\c -> if c `elem` "{-#}" then ' ' else c) (Text.unpack (Red.text t))), w `elem` ["OVERLAPPING", "OVERLAPPABLE", "OVERLAPS"]] of
  w : _ -> Just w
  [] -> Nothing

classDecl :: Red.Red -> (Type, [Decl])
classDecl r = (child 0 r, concat [children w | w <- Red.nodes r, Red.kind w == K.WhereClause])

-- | One equation of a function or a variable: its name, the patterns of
-- its arguments and its right-hand side. An equation written infix,
-- @x + y = ...@, is the same thing.
pattern Clause :: Name -> [Pat] -> Rhs -> Decl
pattern Clause name pats body <- (kindIs [K.FunClause, K.InfixClause] -> Just (clause -> (name, pats, body)))

clause :: Red.Red -> (Name, [Pat], Rhs)
clause r = (name, children r, child 0 r)
  where
    name = case [n | n <- Red.nodes r, Red.kind n `elem` [K.Binder, K.Operator]] of
      n : _ -> nameOf n
      [] -> ""

-- | A pattern binding: @(a, b) = ...@.
pattern PatBind :: Pat -> Rhs -> Decl
pattern PatBind pat body <- (kindIs [K.PatBind] -> Just (patBind -> (pat, body)))

patBind :: Red.Red -> (Pat, Rhs)
patBind r = (child 0 r, child 0 r)

-- | A type signature, for one name or several.
pattern Signature :: [Name] -> Type -> Decl
pattern Signature names ty <- (kindIs [K.TypeSig] -> Just (signature -> (names, ty)))

signature :: Red.Red -> ([Name], Type)
signature r = ([nameOf n | n <- Red.nodes r, Red.kind n == K.Binder], child 0 r)

-- | A fixity declaration: 9 if it gives no precedence.
pattern Fixity :: Assoc -> Int -> [Name] -> Decl
pattern Fixity assoc precedence operators <- (kindIs [K.FixityDecl] -> Just (fixity -> (assoc, precedence, operators)))

fixity :: Red.Red -> (Assoc, Int, [Name])
fixity r = (assoc, precedence, [nameOf o | o <- Red.nodes r, Red.kind o == K.Operator])
  where
    kinds = map Red.kind (Red.tokens r)
    assoc
      | K.KwInfixl `elem` kinds = LeftAssoc
      | K.KwInfixr `elem` kinds = RightAssoc
      | otherwise = NonAssoc
    precedence = case [t | t <- Red.tokens r, Red.kind t == K.IntegerLit] of
      t : _ | Just n <- readMaybe (Text.unpack (Red.text t)) -> n
      _ -> 9

-- | A type synonym: its name, its parameters and what it stands for.
pattern TypeSynonym :: Name -> [Name] -> Type -> Decl
pattern TypeSynonym name parameters ty <- (kindIs [K.TypeDecl] -> Just (synonym -> (name, parameters, ty)))

synonym :: Red.Red -> (Name, [Name], Type)
synonym r = (typeNameOf r, parametersOf r, child 0 r)

-- | The kind of a type, said apart from its declaration.
pattern KindSignature :: Name -> Kind -> Decl
pattern KindSignature name kind <- (kindIs [K.KindSigDecl] -> Just (kindSignature -> Just (name, kind)))

kindSignature :: Red.Red -> Maybe (Name, Kind)
kindSignature r = (,) (typeNameOf r) <$> kindOf r

-- | An import: the module, whether its names come only qualified, the
-- name they are qualified by if it is not the module's, and the items
-- asked for, or if the flag is set those left out.
pattern Import :: Name -> Bool -> Maybe Name -> Maybe (Bool, [Listed]) -> Decl
pattern Import name qualified alias items <- (kindIs [K.ImportDecl] -> Just (importDecl -> (name, qualified, alias, items)))

importDecl :: Red.Red -> (Name, Bool, Maybe Name, Maybe (Bool, [Listed]))
importDecl r = (name, qualified || qualifiedAfter, alias, items)
  where
    -- The package a module is said to be from is not looked at: a module
    -- is found by its name.
    afterImport = drop 1 [t | t@(k, _) <- spelled r, k /= K.StringLit]
    (qualified, afterQualified) = case afterImport of
      (K.VarId, "qualified") : rest -> (True, rest)
      rest -> (False, rest)
    (name, afterName) = case afterQualified of
      (_, n) : rest -> (n, rest)
      [] -> ("", [])
    -- The word may come after the name instead.
    (qualifiedAfter, afterEither) = case afterName of
      (K.VarId, "qualified") : rest -> (True, rest)
      rest -> (False, rest)
    (alias, afterAlias) = case afterEither of
      (K.VarId, "as") : (_, a) : rest -> (Just a, rest)
      rest -> (Nothing, rest)
    items = case afterAlias of
      (K.VarId, "hiding") : (K.LParen, _) : rest -> Just (True, itemsOf rest)
      (K.LParen, _) : rest -> Just (False, itemsOf rest)
      _ -> Nothing

-- | The types a number of no particular type may be taken to be, in the
-- order they are tried.
pattern Default :: [Type] -> Decl
pattern Default types <- (kindIs [K.DefaultDecl] -> Just (children -> types))

-- | A C function brought in under a name, at a type: what C calls it, if
-- that is said, the name, and the type.
pattern Foreign :: Maybe String -> Name -> Type -> Decl
pattern Foreign entity name ty <- (kindIs [K.ForeignDecl] -> Just (foreignDecl -> (entity, name, ty)))

foreignDecl :: Red.Red -> (Maybe String, Name, Type)
foreignDecl r = (entity, name, child 0 r)
  where
    entity = case [t | (K.StringLit, t) <- spelled r] of
      t : _ -> readMaybe t
      [] -> Nothing
    name = case [n | n <- Red.nodes r, Red.kind n == K.Binder] of
      n : _ -> nameOf n
      [] -> ""

-- | A function made C's to call, under a name: what C is to call it, if
-- that is said, the function, and the type C sees it at.
pattern ForeignExport :: Maybe String -> Name -> Type -> Decl
pattern ForeignExport entity name ty <- (kindIs [K.ForeignExportDecl] -> Just (foreignDecl -> (entity, name, ty)))

-- | A pattern synonym: its name, the variables it is applied to, whether
-- it is an expression as well as a pattern, and the pattern it stands
-- for.
pattern PatternSynonym :: Name -> [Name] -> Bool -> Pat -> Decl
pattern PatternSynonym name parameters both definition <- (kindIs [K.PatSynDecl] -> Just (patternSynonym -> (name, parameters, both, definition)))

patternSynonym :: Red.Red -> (Name, [Name], Bool, Pat)
patternSynonym r = (name, parameters, K.Equals `elem` map fst direct, child 0 r)
  where
    direct = spelled r
    name = case [t | (k, t) <- direct, k `elem` [K.ConId, K.ConSym, K.Colon]] of
      t : _ -> t
      [] -> ""
    -- The first is the word pattern.
    parameters = drop 1 [t | (K.VarId, t) <- direct]

-- | The equations after a pattern synonym's where, which say how it is
-- built: for each, the synonym applied to patterns, and what that is.
patternBuilder :: Decl -> [(Pat, Rhs)]
patternBuilder (syntax -> r) = [(child 0 d, child 0 d) | w <- Red.nodes r, Red.kind w == K.WhereClause, d <- Red.nodes w, Red.kind d == K.PatBind]

-- | The type of a pattern synonym, which is not looked at: the synonym's
-- type is what its pattern has.
pattern PatternSignature :: Decl
pattern PatternSignature <- (kindIs [K.PatSynSig] -> Just _)

{-# COMPLETE Data, Clause, PatBind, Signature, Fixity, TypeSynonym, Class, Instance, Import, Default, Foreign, ForeignExport, PatternSynonym, PatternSignature, StandaloneDeriving, KindSignature, RoleAnnotation #-}

-- | What a type's parameters are to it, which is read and not used.
pattern RoleAnnotation :: Decl
pattern RoleAnnotation <- (kindIs [K.RoleDecl] -> Just _)
