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
    Listed (..),
    Subordinates (..),
    itemsOf,
    fieldBinds,
    constructorNameOf,
    spelled,
    children,
    child,
    nameOf,
    literalOf,
  )
where

import Data.Maybe (listToMaybe, mapMaybe)
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
  cast = wrap DeclNode [K.DataDecl, K.TypeDecl, K.ClassDecl, K.InstanceDecl, K.DefaultDecl, K.ForeignDecl, K.ImportDecl, K.TypeSig, K.FixityDecl, K.FunClause, K.InfixClause, K.PatBind]
  syntax (DeclNode r) = r

instance Ast Constructor where
  cast = wrap ConstructorNode [K.ConDecl]
  syntax (ConstructorNode r) = r

instance Ast Rhs where
  cast = wrap RhsNode [K.Rhs]
  syntax (RhsNode r) = r

instance Ast Type where
  cast = wrap TypeNode [K.TyVar, K.TyCon, K.TyApp, K.TyFun, K.TyTuple, K.TyList, K.TyParen, K.TyQual]
  syntax (TypeNode r) = r

instance Ast Expr where
  cast =
    wrap
      ExprNode
      [ K.VarExpr, K.ConExpr, K.LitExpr, K.AppExpr, K.InfixExpr, K.LamExpr, K.LetExpr, K.IfExpr,
        K.CaseExpr, K.DoExpr, K.ParenExpr, K.TupleExpr, K.ListExpr, K.RangeExpr, K.SectionL, K.SectionR, K.TypedExpr, K.RangeStepExpr, K.ListComp, K.RecordExpr
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
  cast = wrap PatNode [K.VarPat, K.WildPat, K.LitPat, K.NegLitPat, K.ConPat, K.InfixPat, K.AsPat, K.ParenPat, K.TuplePat, K.ListPat, K.LazyPat, K.RecordPat]
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
     | otherwise -> "[]"
  name -> name
  where
    kinds = map fst (spelled r)

-- | The @field = value@ bindings directly under a node.
fieldBinds :: (Ast a) => Red -> [(Name, a)]
fieldBinds r = [(nameOf f, child 0 f) | f <- Red.nodes r, Red.kind f == K.FieldBind]

-- | The tokens of a node that mean something, each with its text.
spelled :: Red -> [(SyntaxKind, Name)]
spelled r = [(Red.kind t, Text.unpack (Red.text t)) | t <- Red.tokens r, not (K.isTrivia (Red.kind t))]

-- | The items of a parenthesised list, from the token after its opening
-- parenthesis.
itemsOf :: [(SyntaxKind, Name)] -> [Listed]
itemsOf = \case
  (k, x) : rest | k `elem` [K.VarId, K.QVarId] -> Value x : itemsOf rest
  (K.LParen, _) : (k, x) : (K.RParen, _) : rest | k `elem` [K.VarSym, K.QVarSym] -> Value x : itemsOf rest
  (k, c) : (K.LParen, _) : (K.DotDot, _) : (K.RParen, _) : rest | k `elem` constructors -> Thing c All : itemsOf rest
  (k, c) : (K.LParen, _) : rest | k `elem` constructors ->
    let (inside, after) = break ((== K.RParen) . fst) rest
     in Thing c (Some [x | (k', x) <- inside, k' `elem` [K.VarId, K.ConId, K.VarSym, K.ConSym]]) : itemsOf (drop 1 after)
  (k, c) : rest | k `elem` constructors -> Thing c None : itemsOf rest
  (K.KwModule, _) : (_, m) : rest -> Reexport m : itemsOf rest
  (K.RParen, _) : _ -> []
  _ : rest -> itemsOf rest
  [] -> []
  where
    constructors = [K.ConId, K.QConId]

data Literal
  = Number Integer
  | -- | A floating-point literal, as written.
    Fraction String
  | Character Char
  | Text String
  | -- | A literal with an escape that means nothing, as written.
    Malformed String
  deriving (Eq, Show)

-- | The value of a node's literal token.
literalOf :: Red -> Literal
literalOf r = case [t | t <- Red.tokens r, Red.kind t `elem` [K.IntegerLit, K.FloatLit, K.CharLit, K.StringLit]] of
  t : _ ->
    let s = Text.unpack (Red.text t)
        reading :: (Read a) => (a -> Literal) -> Literal
        reading f = maybe (Malformed s) f (readMaybe s)
     in case Red.kind t of
          K.IntegerLit -> reading Number
          K.CharLit -> reading Character
          K.StringLit -> reading Text
          _ -> Fraction s
  [] -> Malformed ""
