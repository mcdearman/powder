module Stmt (Stmt(..), runStmts) where

import Expr (Expr, evalExpr)
import Size (Size(..))

data Stmt = Assign String Expr
  deriving Show

runStmts :: [(String, Int)] -> [Stmt] -> [(String, Int)]
runStmts env [] = env
runStmts env (Assign v e : rest) = runStmts ((v, evalExpr env e) : env) rest

instance Size Stmt where
  size (Assign _ e) = 1 + size e
