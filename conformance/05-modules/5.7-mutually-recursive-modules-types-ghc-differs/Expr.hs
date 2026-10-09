module Expr (Expr(..), evalExpr) where

import Stmt (Stmt, runStmts)
import Size (Size(..))

data Expr
  = Lit Int
  | Var String
  | Add Expr Expr
  | Block [Stmt] Expr
  deriving Show

evalExpr :: [(String, Int)] -> Expr -> Int
evalExpr _ (Lit n) = n
evalExpr env (Var v) = maybe 0 id (lookup v env)
evalExpr env (Add a b) = evalExpr env a + evalExpr env b
evalExpr env (Block ss e) = evalExpr (runStmts env ss) e

instance Size Expr where
  size (Lit _) = 1
  size (Var _) = 1
  size (Add a b) = 1 + size a + size b
  size (Block ss e) = 1 + size ss + size e
