-- Haskell 2010 Report 5 (intro) / 5.7 / 4.6: mutually recursive data types, a class and its instances spread over a module cycle.
-- GHC only supports this with .hs-boot files, so the expected output (expected-output.txt) was derived by hand.
module Main (main) where

import Expr (Expr(..), evalExpr)
import Stmt (Stmt(..), runStmts)
import Size (Size(..))

program :: [Stmt]
program =
  [ Assign "x" (Lit 5)
  , Assign "y" (Add (Var "x") (Lit 1))
  , Assign "z" (Block [Assign "x" (Lit 100)] (Add (Var "x") (Var "y")))
  ]

main :: IO ()
main = do
  print ("eval literal", evalExpr [] (Add (Lit 1) (Lit 2)))
  print ("eval var", evalExpr [("a", 7)] (Var "a"))
  print ("run", runStmts [] program)
  print ("size expr", size (Add (Lit 1) (Var "q")))
  print ("size stmt", size (Assign "x" (Lit 5)))
  print ("size program", size program)
  print ("show", Assign "k" (Block [Assign "j" (Lit 1)] (Var "j")))
