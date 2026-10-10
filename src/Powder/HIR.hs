-- | HIR: the program after name resolution.
--
-- It has the shape of the syntax, less a little sugar: operators are
-- applications, grouped by their fixities; sections and @do@ are lambdas
-- and applications; tuples and lists are constructors; the equations of a
-- function are one binding. What it adds is identity. Every node has a
-- 'NodeId' and every name a 'NameId', so nothing else needs to be stored
-- in the tree: what is known about a node or a name lives in a table
-- beside it, keyed by its identity. Where each node came from in the
-- source is such a table, and what each name is another. A type checker's
-- result is one more: the type of each node, under its 'NodeId'.
--
-- Patterns are in "Powder.HIR.Pat", to be imported qualified, since they
-- share the names of their forms with expressions.
module Powder.HIR
  ( module Powder.HIR.Id,
    Program (..),
    chunk,
    mine,
    NameInfo (..),
    NameSort (..),
    Origin (..),
    Signature (..),
    Constraint (..),
    Class (..),
    Instance (..),
    DataType (..),
    Synonym (..),
    Builtins (..),
    Binding (..),
    BindingKind (..),
    Clause (..),
    Rhs (..),
    Guarded (..),
    Qualifier (..),
    Alt (..),
    Expr (..),
    ExprKind (..),
    Pat,
    nameInfo,
    origin,
    signature,
    classOf,
    allBindings,
    nodeIds,
    bound,
    mentions,
    pretty,
  )
where

import qualified Data.IntMap.Strict as IntMap
import Data.List (intercalate)
import Powder.Core (ConInfo, Prim, primName)
import Powder.HIR.Id
import Powder.HIR.Pat (Pat (..))
import qualified Powder.HIR.Pat as Pat
import Powder.HIR.Type (Type)
import Powder.Syntax.Token (Pos)

data Program = Program
  { -- | The top-level definitions, in the order of the source.
    programBindings :: [Binding],
    -- | What each name is, by 'NameId'.
    programNames :: Table NameInfo,
    -- | Where each node came from, by 'NodeId'.
    programOrigins :: Table Origin,
    -- | The type each name was declared to have, if it was, by 'NameId'.
    programSignatures :: Table Signature,
    programDataTypes :: [DataType],
    -- | What each type synonym stands for, by 'NameId'.
    programSynonyms :: Table Synonym,
    -- | What each class is, by 'NameId'.
    programClasses :: Table Class,
    programInstances :: [Instance],
    programBuiltins :: Builtins,
    -- | The module whose names are spelled plainly in what is compiled.
    programRoot :: String,
    -- | The types each file's default declaration names, if it has one.
    programDefaults :: [(FilePath, [Type])],
    -- | The constructors of newtypes, which are not there when a program
    -- runs: one is what it holds.
    programNewtypes :: [NameId],
    -- | The module each file is, or is part of, and the modules each
    -- module imports.
    programModules :: [(FilePath, String)],
    programImports :: [(String, [String])],
    -- | Where the identities made for this program start, when it is a
    -- package compiled after others: what is below, or a 'chunk' or more
    -- above, is another package's, here to be known about and not to be
    -- compiled. Negative when the program is all there is.
    programFloor :: Int
  }

-- | How many identities a package may make: each package's are in a run
-- of this many, so that packages compiled apart do not make the same ones.
chunk :: Int
chunk = 2 ^ (24 :: Int)

-- | Whether an identity is one this program made, and not one of a
-- package it was compiled after.
mine :: Program -> Int -> Bool
mine program k = floor' < 0 || (k >= floor' && k < floor' + chunk)
  where
    floor' = programFloor program

-- | A class asked of a type: one part of a context.
data Constraint = Constraint {constraintClass :: NameId, constraintType :: Type}

-- | A class. A method is a name like any other, with a signature that
-- quantifies over the class's variable first and has no mention of the
-- class in its context: that it is a method says so.
data Class = Class
  { className :: NameId,
    classVariable :: NameId,
    -- | The classes every instance must also be an instance of.
    classSupers :: [NameId],
    classMethods :: [NameId],
    -- | The definitions a method has where an instance gives none: each
    -- the 'Function' of a method.
    classDefaults :: [Binding]
  }

-- | An instance of a class, at a type constructor applied to variables.
data Instance = Instance
  { instanceId :: NodeId,
    instanceClass :: NameId,
    instanceType :: NameId,
    instanceVariables :: [NameId],
    -- | What it needs of its variables.
    instanceContext :: [Constraint],
    -- | Each the 'Function' of a method of the class.
    instanceMethods :: [Binding]
  }

-- | A declared type. Its type variables are its own: every signature
-- quantifies over the variables it mentions.
data Signature = Signature
  { signatureVariables :: [NameId],
    signatureContext :: [Constraint],
    signatureType :: Type
  }

-- | A data type: its parameters, and each constructor with the types of
-- its fields.
data DataType = DataType
  { dataName :: NameId,
    dataParameters :: [NameId],
    dataConstructors :: [(NameId, [Type])]
  }

data Synonym = Synonym {synonymParameters :: [NameId], synonymType :: Type}

-- | The types the language itself refers to: those of literals, of
-- conditions, and of what syntax builds.
data Builtins = Builtins
  { builtinInteger :: NameId,
    builtinChar :: NameId,
    builtinIO :: NameId,
    builtinBool :: NameId,
    builtinList :: NameId,
    builtinUnit :: NameId,
    -- | The fixed-size integer, which for now is the other underneath.
    builtinInt :: NameId,
    -- | What a number written in a program needs: the class of numbers
    -- and its method that makes one of an integer, and to be matched
    -- against, the class with equality and that method. They are the
    -- prelude's, and without one a number is an 'Integer'.
    builtinNumeric :: Maybe (NameId, NameId),
    builtinEquality :: Maybe (NameId, NameId),
    -- | The floating-point types, and what a number written with a
    -- fraction needs: the class of numbers that have fractions, its
    -- method that makes one of a ratio, and the constructor of ratios.
    builtinDouble :: NameId,
    builtinFloat :: NameId,
    builtinFractional :: Maybe (NameId, NameId, NameId)
  }

data NameInfo = NameInfo
  { -- | As it is spelled in the source.
    nameText :: String,
    nameSort :: NameSort,
    -- | The module it is declared in, if it is declared at the top of one.
    nameModule :: String
  }

data NameSort
  = -- | Defined at the top level.
    Global
  | -- | Bound by a pattern or a local definition.
    Local
  | -- | A data constructor: its place in its type, and all that type's
    -- constructors in order.
    Constructor ConInfo [NameId]
  | TypeConstructor
  | TypeClass
  | -- | A method of a class.
    Method NameId
  | TypeSynonym
  | TypeVariable

-- | The piece of source a node stands for.
data Origin = Origin {originFile :: FilePath, originSpan :: (Pos, Pos)}

data Binding = Binding {bindingId :: NodeId, bindingKind :: BindingKind}

data BindingKind
  = -- | A name and its equations. A variable is a function of no arguments
    -- with one equation.
    Function NameId [Clause]
  | -- | @pattern = ...@: binds the names in the pattern.
    Destructure Pat Rhs

-- | One equation of a function: the patterns of its arguments and what it
-- is equal to when they match.
data Clause = Clause {clauseId :: NodeId, clausePats :: [Pat], clauseRhs :: Rhs}

-- | A right-hand side: alternatives tried in order, and the local
-- definitions in scope over all of them.
data Rhs = Rhs {rhsId :: NodeId, rhsGuarded :: [Guarded], rhsWhere :: [Binding]}

-- | A body, and the guard it is chosen by: qualifiers that must all hold,
-- each in scope of what those before it bind. With none it is chosen.
data Guarded = Guarded {guardedId :: NodeId, guardedQualifiers :: [Qualifier], guardedBody :: Expr}

data Qualifier
  = -- | Holds if it is true.
    Condition Expr
  | -- | Holds if the expression matches the pattern, whose names it binds.
    Match Pat Expr
  | -- | Always holds. Binds.
    Bindings [Binding]

-- | A case alternative.
data Alt = Alt {altId :: NodeId, altPat :: Pat, altRhs :: Rhs}

data Expr = Expr {exprId :: NodeId, exprKind :: ExprKind}

data ExprKind
  = Var NameId
  | Con NameId
  | -- | A primitive operation.
    Prim Prim
  | Lit Literal
  | App Expr Expr
  | Lam [Pat] Expr
  | -- | Local definitions, all in scope in each other and in the body.
    Let [Binding] Expr
  | If Expr Expr Expr
  | Case Expr [Alt]
  | -- | An expression said to have a type.
    Annotated Expr Signature
  | -- | A C function, by what C calls it. What it takes and gives is in
    -- the signature of the name it is bound to.
    Foreign String
  | -- | Where the source was wrong.
    Error

nameInfo :: Program -> NameId -> Maybe NameInfo
nameInfo program (NameId n) = IntMap.lookup n (programNames program)

origin :: Program -> NodeId -> Maybe Origin
origin program (NodeId n) = IntMap.lookup n (programOrigins program)

signature :: Program -> NameId -> Maybe Signature
signature program (NameId n) = IntMap.lookup n (programSignatures program)

classOf :: Program -> NameId -> Maybe Class
classOf program (NameId n) = IntMap.lookup n (programClasses program)

-- | Every binding at the top of a program: its definitions, the defaults
-- of its classes and the methods of its instances.
allBindings :: Program -> [Binding]
allBindings program =
  programBindings program
    ++ concatMap classDefaults (IntMap.elems (programClasses program))
    ++ concatMap instanceMethods (programInstances program)

-- | The names a binding defines.
bound :: Binding -> [NameId]
bound b = case bindingKind b of
  Function f _ -> [f]
  Destructure p _ -> Pat.binders p

-- | The identity of every node in a program, in the order of the source.
nodeIds :: Program -> [NodeId]
nodeIds = concatMap binding . allBindings
  where
    binding b = bindingId b : case bindingKind b of
      Function _ clauses -> concatMap clause clauses
      Destructure p r -> pat p ++ rhs r
    clause c = clauseId c : concatMap pat (clausePats c) ++ rhs (clauseRhs c)
    rhs r = rhsId r : concatMap guarded (rhsGuarded r) ++ concatMap binding (rhsWhere r)
    guarded g = guardedId g : concatMap qualifier (guardedQualifiers g) ++ expr (guardedBody g)
    qualifier = \case
      Condition c -> expr c
      Match p e -> pat p ++ expr e
      Bindings bs -> concatMap binding bs
    alt a = altId a : pat (altPat a) ++ rhs (altRhs a)
    pat p = patId p : concatMap pat (Pat.subpatterns p)
    expr e = exprId e : case exprKind e of
      App f a -> expr f ++ expr a
      Lam ps body -> concatMap pat ps ++ expr body
      Let bs body -> concatMap binding bs ++ expr body
      If c t f -> expr c ++ expr t ++ expr f
      Case scrutinee alts -> expr scrutinee ++ concatMap alt alts
      Annotated inner _ -> expr inner
      _ -> []

-- | The variables a binding refers to, its own among them if it is
-- recursive.
mentions :: Binding -> [NameId]
mentions = binding
  where
    binding b = case bindingKind b of
      Function _ clauses -> concatMap (rhs . clauseRhs) clauses
      Destructure _ r -> rhs r
    rhs r = concatMap guarded (rhsGuarded r) ++ concatMap binding (rhsWhere r)
    guarded g = concatMap qualifier (guardedQualifiers g) ++ expr (guardedBody g)
    qualifier = \case
      Condition c -> expr c
      Match _ e -> expr e
      Bindings bs -> concatMap binding bs
    expr e = case exprKind e of
      Var x -> [x]
      App f a -> expr f ++ expr a
      Lam _ body -> expr body
      Let bs body -> concatMap binding bs ++ expr body
      If c t f -> expr c ++ expr t ++ expr f
      Case scrutinee alts -> expr scrutinee ++ concatMap (rhs . altRhs) alts
      Annotated inner _ -> expr inner
      _ -> []

-- Pretty printing -----------------------------------------------------------

-- | A program as text: each name with its identity after it, so that two
-- names spelled alike can be told apart.
pretty :: Program -> String
pretty program = unlines (map (binding 0) (programBindings program))
  where
    name n = maybe "?" nameText (nameInfo program n) ++ "#" ++ show (nameKey n)
    indent n = replicate (2 * n) ' '

    binding :: Int -> Binding -> String
    binding depth b = case bindingKind b of
      Function f clauses -> intercalate "\n" [indent depth ++ unwords (name f : map (pat True) (clausePats c)) ++ rhs depth " =" (clauseRhs c) | c <- clauses]
      Destructure p r -> indent depth ++ pat False p ++ rhs depth " =" r

    rhs depth sep r = concatMap guarded (rhsGuarded r) ++ wheres
      where
        guarded g = case guardedQualifiers g of
          [] -> sep ++ " " ++ expr False (guardedBody g)
          qs -> "\n" ++ indent (depth + 1) ++ "| " ++ intercalate ", " (map qualifier qs) ++ sep ++ " " ++ expr False (guardedBody g)
        qualifier = \case
          Condition c -> expr False c
          Match p e -> pat False p ++ " <- " ++ expr False e
          Bindings bs -> "let { " ++ intercalate "; " (map (binding 0) bs) ++ " }"
        wheres
          | null (rhsWhere r) = ""
          | otherwise = "\n" ++ indent (depth + 1) ++ "where\n" ++ intercalate "\n" (map (binding (depth + 2)) (rhsWhere r))

    parens atomic s = if atomic then "(" ++ s ++ ")" else s

    pat :: Bool -> Pat -> String
    pat atomic p = case patKind p of
      Pat.Var x -> name x
      Pat.Wild -> "_"
      Pat.Lit l -> literal l
      Pat.Con c [] -> name c
      Pat.Con c ps -> parens atomic (unwords (name c : map (pat True) ps))
      Pat.As x q -> name x ++ "@" ++ pat True q
      Pat.Lazy q -> "~" ++ pat True q
      Pat.Ordered _ q -> pat atomic q
      Pat.Bang q -> "!" ++ pat True q
      Pat.Error -> "<error>"

    literal = \case
      Number n -> show n
      Fraction q -> show (fromRational q :: Double)
      Character c -> show c
      Text s -> show s

    expr :: Bool -> Expr -> String
    expr atomic e = case exprKind e of
      Var x -> name x
      Con c -> name c
      Prim p -> primName p
      Lit l -> literal l
      App f a -> parens atomic (expr False f ++ " " ++ expr True a)
      Lam ps body -> parens atomic ("\\" ++ unwords (map (pat True) ps) ++ " -> " ++ expr False body)
      Let bs body -> parens atomic ("let { " ++ intercalate "; " (map (binding 0) bs) ++ " } in " ++ expr False body)
      If c t f -> parens atomic ("if " ++ expr False c ++ " then " ++ expr False t ++ " else " ++ expr False f)
      Case scrutinee alts -> parens atomic ("case " ++ expr False scrutinee ++ " of { " ++ intercalate "; " [pat False (altPat a) ++ rhs 0 " ->" (altRhs a) | a <- alts] ++ " }")
      Annotated inner _ -> expr atomic inner
      Foreign symbol -> "<foreign " ++ symbol ++ ">"
      Error -> "<error>"
