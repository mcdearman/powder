{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | The forms of an expression. Import qualified.
module Powder.Syntax.AST.Expr
  ( Expr (Var, Con, Lit, App, Infix, Lam, Let, If, Case, Do, Paren, Tuple, List, Range, SectionL, SectionR, Typed, Comprehension, Record, LamCase, MultiIf, TupleSection),
    Item (..),
    recordWild,
    comprehensionBranches,
  )
where

import Data.Maybe (listToMaybe, mapMaybe)
import Powder.Syntax.AST.Internal
import qualified Powder.Syntax.Kind as K
import qualified Powder.Syntax.Red as Red

kindIs :: K.SyntaxKind -> Expr -> Maybe Red.Red
kindIs k (syntax -> r) = if Red.kind r == k then Just r else Nothing

-- | A variable, or an operator in parentheses.
pattern Var :: Name -> Expr
pattern Var name <- (kindIs K.VarExpr -> Just (nameOf -> name))

-- | A constructor, or a constructor operator in parentheses.
pattern Con :: Name -> Expr
pattern Con name <- (kindIs K.ConExpr -> Just (constructorNameOf -> name))

pattern Lit :: Literal -> Expr
pattern Lit literal <- (kindIs K.LitExpr -> Just (literalOf -> literal))

-- | A function and the arguments it is applied to, of which there is at
-- least one.
pattern App :: Expr -> [Expr] -> Expr
pattern App function arguments <- (kindIs K.AppExpr -> Just (children -> function : arguments))

-- | What stands in a row in an infix expression.
data Item
  = Operand Expr
  | Operator Operator
  | -- | A prefix minus, which applies to as much of what follows as binds
    -- tighter than subtraction.
    Negate

-- | Operands and operators as written, not yet grouped by precedence.
pattern Infix :: [Item] -> Expr
pattern Infix items <- (kindIs K.InfixExpr -> Just (items' -> items))

items' :: Red.Red -> [Item]
items' = mapMaybe item . Red.nodes
  where
    item n
      | Red.kind n == K.Negation = Just Negate
      | Just o <- cast n = Just (Operator o)
      | otherwise = Operand <$> cast n

pattern Lam :: [Pat] -> Expr -> Expr
pattern Lam pats body <- (kindIs K.LamExpr -> Just (binding -> (pats, body)))

pattern Let :: [Decl] -> Expr -> Expr
pattern Let decls body <- (kindIs K.LetExpr -> Just (binding -> (decls, body)))

-- | What a node binds, and the expression it binds it in.
binding :: (Ast a) => Red.Red -> ([a], Expr)
binding r = (children r, child 0 r)

pattern If :: Expr -> Expr -> Expr -> Expr
pattern If condition true false <- (kindIs K.IfExpr -> Just (children -> [condition, true, false]))

pattern Case :: Expr -> [Alt] -> Expr
pattern Case scrutinee alts <- (kindIs K.CaseExpr -> Just (binding' -> (scrutinee, alts)))

binding' :: (Ast a) => Red.Red -> (Expr, [a])
binding' r = (child 0 r, children r)

pattern Do :: [Stmt] -> Expr
pattern Do stmts <- (kindIs K.DoExpr -> Just (children -> stmts))

pattern Paren :: Expr -> Expr
pattern Paren inner <- (kindIs K.ParenExpr -> Just (child 0 -> inner))

-- | A tuple, or with no components the unit.
pattern Tuple :: [Expr] -> Expr
pattern Tuple components <- (kindIs K.TupleExpr -> Just (children -> components))

pattern List :: [Expr] -> Expr
pattern List elements <- (kindIs K.ListExpr -> Just (children -> elements))

-- | @[from ..]@, @[from .. to]@, @[from, next ..]@ or @[from, next .. to]@.
pattern Range :: Expr -> Maybe Expr -> Maybe Expr -> Expr
pattern Range from next to <- (range -> Just (from, next, to))

range :: Expr -> Maybe (Expr, Maybe Expr, Maybe Expr)
range (syntax -> r)
  | Red.kind r == K.RangeExpr = Just (child 0 r, Nothing, listToMaybe (drop 1 (children r)))
  | Red.kind r == K.RangeStepExpr = Just (child 0 r, Just (child 1 r), listToMaybe (drop 2 (children r)))
  | otherwise = Nothing

-- | A record built, if what is before the braces is a constructor, or
-- else a record with some fields changed.
pattern Record :: Expr -> [(Name, Maybe Expr)] -> Expr
pattern Record subject fields <- (kindIs K.RecordExpr -> Just (record -> (subject, fields)))

-- | Whether a record's fields end in @..@: each field not named is given
-- the variable of its name, where there is one.
recordWild :: Expr -> Bool
recordWild = fieldWild . syntax

record :: Red.Red -> (Expr, [(Name, Maybe Expr)])
record r = (child 0 r, fieldBinds r)

-- | A list comprehension: what each element is, and the qualifiers, which
-- are generators, local definitions and conditions.
pattern Comprehension :: Expr -> [Stmt] -> Expr
pattern Comprehension element qualifiers <- (kindIs K.ListComp -> Just (comprehension -> (element, qualifiers)))

comprehension :: Red.Red -> (Expr, [Stmt])
comprehension r = (child 0 r, children r)

-- | The qualifiers a list comprehension goes through beside its first:
-- those after each bar but the first.
comprehensionBranches :: Expr -> [[Stmt]]
comprehensionBranches (syntax -> r) = [children b | b <- Red.nodes r, Red.kind b == K.ListCompBranch]

-- | @(e op)@
pattern SectionL :: Expr -> Operator -> Expr
pattern SectionL operand operator <- (kindIs K.SectionL -> Just (section -> (operator, operand)))

-- | @(op e)@
pattern SectionR :: Operator -> Expr -> Expr
pattern SectionR operator operand <- (kindIs K.SectionR -> Just (section -> (operator, operand)))

section :: Red.Red -> (Operator, Expr)
section r = (child 0 r, child 0 r)

-- | An expression said to have a type: @e :: t@.
pattern Typed :: Expr -> Type -> Expr
pattern Typed inner ty <- (kindIs K.TypedExpr -> Just (typedExpr -> (inner, ty)))

typedExpr :: Red.Red -> (Expr, Type)
typedExpr r = (child 0 r, child 0 r)

-- | @\\case alternatives@: a function of one argument, which it takes
-- apart.
pattern LamCase :: [Alt] -> Expr
pattern LamCase alts <- (kindIs K.LamCaseExpr -> Just (children -> alts))

-- | @if | guard -> e | ...@: the first whose guard holds.
pattern MultiIf :: Rhs -> Expr
pattern MultiIf alternatives <- (kindIs K.MultiIfExpr -> Just (child 0 -> alternatives))

-- | A tuple with some of its places left out, which is a function of
-- those: @(,x)@.
pattern TupleSection :: [Maybe Expr] -> Expr
pattern TupleSection places <- (kindIs K.TupleSection -> Just (places' -> places))

places' :: Red.Red -> [Maybe Expr]
places' r = [if Red.kind n == K.TupleHole then Nothing else cast n | n <- Red.nodes r]

{-# COMPLETE Var, Con, Lit, App, Infix, Lam, Let, If, Case, Do, Paren, Tuple, List, Range, SectionL, SectionR, Typed, Comprehension, Record, LamCase, MultiIf, TupleSection #-}
