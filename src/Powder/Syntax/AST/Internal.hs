-- | The typed layer's plumbing: each sort of syntax as a wrapper around a
-- red node, and how to find things under one.
--
-- This module is for the modules of this layer only. Everything else gets
-- a node by 'cast', which checks its kind, and takes it apart with the
-- pattern synonyms those modules export.
module Powder.Syntax.AST.Internal
  ( Name,
    Ast (..),
    Module,
    Decl,
    Constructor,
    Rhs,
    Type,
    Expr,
    Operator,
    Alt,
    Stmt,
    Pat,
    Literal (..),
    Kind (..),
    kindOf,
    kindedBinders,
    Listed (..),
    Subordinates (..),
    itemsOf,
    fieldBinds,
    fieldWild,
    constructorNameOf,
    spelled,
    children,
    child,
    nameOf,
    literalOf,
  )
where

import Data.Bifunctor (first)
import Data.Char (digitToInt)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text as Text
import Powder.Syntax.Kind (SyntaxKind)
import qualified Powder.Syntax.Kind as K
import Powder.Syntax.Red (Red)
import qualified Powder.Syntax.Red as Red
import Text.Read (readMaybe)

type Name = String

-- | A sort of syntax: a red node of one of some kinds.
class Ast a where
  -- | The node as this sort of syntax, if it is one.
  cast :: Red -> Maybe a

  -- | The node underneath.
  syntax :: a -> Red

newtype Module = ModuleNode Red

newtype Decl = DeclNode Red

-- | A constructor in a data declaration.
newtype Constructor = ConstructorNode Red

newtype Rhs = RhsNode Red

newtype Type = TypeNode Red

newtype Expr = ExprNode Red

newtype Operator = OperatorNode Red

newtype Alt = AltNode Red

newtype Stmt = StmtNode Red

newtype Pat = PatNode Red

wrap :: (Red -> a) -> [SyntaxKind] -> Red -> Maybe a
wrap f kinds r = if Red.kind r `elem` kinds then Just (f r) else Nothing

instance Ast Module where
  cast = wrap ModuleNode [K.Module]
  syntax (ModuleNode r) = r

instance Ast Decl where
  cast = wrap DeclNode [K.TypeFamilyDecl, K.TypeInstanceDecl, K.RoleDecl, K.KindSigDecl, K.StandaloneDerivingDecl, K.PatSynDecl, K.PatSynSig, K.DataDecl, K.TypeDecl, K.ClassDecl, K.InstanceDecl, K.DefaultDecl, K.ForeignDecl, K.ForeignExportDecl, K.ImportDecl, K.TypeSig, K.FixityDecl, K.FunClause, K.InfixClause, K.PatBind]
  syntax (DeclNode r) = r

instance Ast Constructor where
  cast = wrap ConstructorNode [K.ConDecl]
  syntax (ConstructorNode r) = r

instance Ast Rhs where
  cast = wrap RhsNode [K.Rhs]
  syntax (RhsNode r) = r

instance Ast Type where
  cast = wrap TypeNode [K.TyVar, K.TyCon, K.TyApp, K.TyFun, K.TyTuple, K.TyList, K.TyParen, K.TyQual, K.TyForall, K.TyWild, K.TyKinded, K.TyInfix, K.TyPromotedList]
  syntax (TypeNode r) = r

instance Ast Expr where
  cast =
    wrap
      ExprNode
      [ K.VarExpr, K.ConExpr, K.LitExpr, K.AppExpr, K.InfixExpr, K.LamExpr, K.LetExpr, K.IfExpr,
        K.CaseExpr, K.DoExpr, K.ParenExpr, K.TupleExpr, K.ListExpr, K.RangeExpr, K.SectionL, K.SectionR, K.TypedExpr, K.RangeStepExpr, K.ListComp, K.RecordExpr, K.LamCaseExpr, K.MultiIfExpr, K.TupleSection
      ]
  syntax (ExprNode r) = r

instance Ast Operator where
  cast = wrap OperatorNode [K.Operator]
  syntax (OperatorNode r) = r

instance Ast Alt where
  cast = wrap AltNode [K.Alt]
  syntax (AltNode r) = r

instance Ast Stmt where
  cast = wrap StmtNode [K.BindStmt, K.LetStmt, K.ExprStmt]
  syntax (StmtNode r) = r

instance Ast Pat where
  cast = wrap PatNode [K.SigPat, K.ViewPat, K.BangPat, K.VarPat, K.WildPat, K.LitPat, K.NegLitPat, K.ConPat, K.InfixPat, K.AsPat, K.ParenPat, K.TuplePat, K.ListPat, K.LazyPat, K.RecordPat]
  syntax (PatNode r) = r

-- | The children of a node that are of some sort.
children :: (Ast a) => Red -> [a]
children = mapMaybe cast . Red.nodes

-- | The n-th child of some sort. The parser makes no tree without it, so
-- its absence is a bug here or there.
child :: (Ast a) => Int -> Red -> a
child n r = case drop n (children r) of
  a : _ -> a
  [] -> error ("Powder.Syntax.AST: a " ++ show (Red.kind r) ++ " node is missing a child")

-- | The name a node spells: its first token that is a name or an operator,
-- so that @x@, @(+)@ and @`div`@ are @x@, @+@ and @div@.
nameOf :: Red -> Name
nameOf r = maybe "" (Text.unpack . Red.text) (listToMaybe [t | t <- Red.tokens r, Red.kind t `elem` names])
  where
    names = [K.VarId, K.ConId, K.QVarId, K.QConId, K.VarSym, K.ConSym, K.QVarSym, K.QConSym, K.Colon, K.Bang, K.Tilde]

-- | One thing an export list or an import names.
data Listed
  = -- | A variable.
    Value Name
  | -- | A type or a class, and which of what belongs to it: its
    -- constructors, or its methods.
    Thing Name Subordinates
  | -- | @module M@, in an export list: all that was imported from it.
    Reexport Name
  | -- | @pattern P@: a pattern synonym.
    Pattern Name
  deriving (Eq, Show)

data Subordinates = None | All | Some [Name]
  deriving (Eq, Show)

-- | The constructor a node spells, which may be one that has no name of
-- its own: a tuple's, written as its commas, the list's, written @[]@,
-- or the function arrow.
constructorNameOf :: Red -> Name
constructorNameOf r = case nameOf r of
  "" | K.RightArrow `elem` kinds -> "->"
     | K.Comma `elem` kinds -> "(" ++ concatMap (filter (== ',') . snd) (spelled r) ++ ")"
     | K.LParen `elem` kinds -> "()"
     -- With a tick, the list's constructor and not its type.
     | "'[" `elem` map snd (spelled r) -> "'[]"
     | otherwise -> "[]"
  name -> name
  where
    kinds = map fst (spelled r)

-- | The @field = value@ bindings directly under a node. One with no
-- value is a pun: the field, as the variable of its name.
fieldBinds :: (Ast a) => Red -> [(Name, Maybe a)]
fieldBinds r = [(nameOf f, listToMaybe (children f)) | f <- Red.nodes r, Red.kind f == K.FieldBind]

-- | Whether a node's field bindings end in @..@, for the fields not named.
fieldWild :: Red -> Bool
fieldWild r = any ((== K.FieldWild) . Red.kind) (Red.nodes r)

-- | A kind as it is written: the kind of types, a function between
-- kinds, a data type whose values are types of it, a variable, or a list.
data Kind = Star | Arrow Kind Kind | Named Name | Variable Name | ListOf Kind
  deriving (Eq, Show)

-- | The kind a node gives: that of the first kind under it. A kind is
-- stars with arrows between them, grouping to the right.
kindOf :: Red -> Maybe Kind
kindOf r = case [k | k <- Red.nodes r, Red.kind k == K.KindSig] of
  k : _ -> fst <$> arrows (spelled k)
  [] -> Nothing
  where
    arrows ts = do
      (left, rest) <- atom ts
      case rest of
        (K.RightArrow, _) : more -> first (Arrow left) <$> arrows more
        _ -> Just (left, rest)
    atom = \case
      (K.LParen, _) : more -> do
        (inner, rest) <- arrows more
        case rest of
          (K.RParen, _) : after -> Just (inner, after)
          _ -> Nothing
      (K.LBracket, _) : more -> do
        (inner, rest) <- arrows more
        case rest of
          (K.RBracket, _) : after -> Just (ListOf inner, after)
          _ -> Nothing
      (K.VarSym, _) : more -> Just (Star, more)
      (K.VarId, v) : more -> Just (Variable v, more)
      (t, c) : more
        | t `elem` [K.ConId, K.QConId] ->
            -- The kind of types by its names, and any other a data type.
            let plain = reverse (takeWhile (/= '.') (reverse c))
             in Just (if plain `elem` ["Type", "Constraint"] then Star else Named c, more)
      _ -> Nothing

-- | The type variables directly under a node that are given kinds, each
-- with its kind.
kindedBinders :: Red -> [(Name, Kind)]
kindedBinders r = [(nameOf b, k) | b <- Red.nodes r, Red.kind b == K.KindedBinder, Just k <- [kindOf b]]

-- | The tokens of a node that mean something, each with its text.
spelled :: Red -> [(SyntaxKind, Name)]
spelled r = [(Red.kind t, Text.unpack (Red.text t)) | t <- Red.tokens r, not (K.isTrivia (Red.kind t))]

-- | The items of a parenthesised list, from the token after its opening
-- parenthesis.
itemsOf :: [(SyntaxKind, Name)] -> [Listed]
itemsOf = \case
  -- After the word type an operator in parentheses is a type's name.
  (K.KwType, _) : (K.LParen, _) : (k, c) : (K.RParen, _) : (K.LParen, _) : (K.DotDot, _) : (K.RParen, _) : rest | k `elem` operators -> Thing c All : itemsOf rest
  (K.KwType, _) : (K.LParen, _) : (k, c) : (K.RParen, _) : rest | k `elem` operators -> Thing c None : itemsOf rest
  -- The word pattern with a constructor's name right after it. Before
  -- anything else it is a variable of that name.
  (K.VarId, "pattern") : (k, c) : rest | k `elem` constructors -> Pattern c : itemsOf rest
  (K.VarId, "pattern") : (K.LParen, _) : (k, c) : (K.RParen, _) : rest | k `elem` [K.ConSym, K.Colon] -> Pattern c : itemsOf rest
  (k, x) : rest | k `elem` [K.VarId, K.QVarId] -> Value x : itemsOf rest
  (K.LParen, _) : (k, x) : (K.RParen, _) : rest | k `elem` [K.VarSym, K.QVarSym] -> Value x : itemsOf rest
  (K.LParen, _) : (k, c) : (K.RParen, _) : (K.LParen, _) : (K.DotDot, _) : (K.RParen, _) : rest | k `elem` [K.ConSym, K.QConSym] -> Thing c All : itemsOf rest
  (K.LParen, _) : (k, c) : (K.RParen, _) : rest | k `elem` [K.ConSym, K.QConSym] -> Thing c None : itemsOf rest
  (k, c) : (K.LParen, _) : (K.DotDot, _) : (K.RParen, _) : rest | k `elem` constructors -> Thing c All : itemsOf rest
  (k, c) : (K.LParen, _) : rest | k `elem` constructors ->
    let (inside, after) = break ((== K.RParen) . fst) rest
     in Thing c (Some ([x | (k', x) <- inside, k' `elem` [K.VarId, K.ConId, K.VarSym, K.ConSym]] ++ [plain x | (k', x) <- inside, k' `elem` [K.QVarId, K.QConId]])) : itemsOf (drop 1 after)
  (k, c) : rest | k `elem` constructors -> Thing c None : itemsOf rest
  (K.KwModule, _) : (_, m) : rest -> Reexport m : itemsOf rest
  -- The word says what the name after it is, which its spelling says too.
  (K.KwType, _) : rest -> itemsOf rest
  (K.RParen, _) : _ -> []
  _ : rest -> itemsOf rest
  [] -> []
  where
    constructors = [K.ConId, K.QConId]
    operators = [K.VarSym, K.QVarSym, K.ConSym, K.QConSym]
    -- What belongs to a type may be named with its module, which says
    -- nothing more than its name does.
    plain = reverse . takeWhile (/= '.') . reverse

data Literal
  = Number Integer
  | -- | A floating-point literal, as written.
    Fraction String
  | Character Char
  | Text String
  | -- | A literal with an escape that means nothing, as written.
    Malformed String
  deriving (Eq, Show)

-- | A fraction written in hexadecimal, as the same number in decimal: it
-- is a whole number of some power of two, which is one of a power of ten.
-- Anything else is left as it is.
hexadecimal :: String -> String
hexadecimal s = case s of
  '0' : x : rest | x `elem` "xX" ->
    let (mantissa, power) = break (`elem` "pP") rest
        (whole, fraction) = break (== '.') mantissa
        digits = whole ++ drop 1 fraction
        n = foldl (\acc d -> 16 * acc + toInteger (digitToInt d)) 0 digits
        e = fromMaybe 0 (readMaybe (dropWhile (== '+') (drop 1 power))) - 4 * (length fraction - min 1 (length fraction))
     in if e >= 0 then show (n * 2 ^ e) ++ ".0" else show (n * 5 ^ negate e) ++ "e-" ++ show (negate e :: Int)
  _ -> s

-- | The value of a node's literal token.
literalOf :: Red -> Literal
literalOf r = case [t | t <- Red.tokens r, Red.kind t `elem` [K.IntegerLit, K.FloatLit, K.CharLit, K.StringLit]] of
  t : _ ->
    let s = Text.unpack (Red.text t)
        reading :: (Read a) => (a -> Literal) -> Literal
        reading f = maybe (Malformed s) f (readMaybe s)
        -- Underscores in a number are for the reader; and a number may be
        -- in binary.
        digits = filter (/= '_') s
        number = case digits of
          '0' : b : bits | b `elem` "bB", not (null bits), all (`elem` "01") bits -> Number (foldl (\n d -> 2 * n + (if d == '1' then 1 else 0)) 0 bits)
          _ -> maybe (Malformed s) Number (readMaybe digits)
     in case Red.kind t of
          K.IntegerLit -> number
          K.CharLit -> reading Character
          K.StringLit -> reading Text
          _ -> Fraction (hexadecimal digits)
  [] -> Malformed ""
