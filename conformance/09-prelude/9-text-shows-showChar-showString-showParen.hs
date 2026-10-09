-- Haskell 2010 Report 9 (PreludeText): ShowS, shows, showChar, showString, showParen and their compositions
module Main (main) where

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr

-- a hand-written precedence-aware printer built from the ShowS combinators
showExpr :: Int -> Expr -> ShowS
showExpr d (Lit n) = showsPrec d n
showExpr d (Add a b) = showParen (d > 6) (showExpr 6 a . showString " + " . showExpr 7 b)
showExpr d (Mul a b) = showParen (d > 7) (showExpr 7 a . showString " * " . showExpr 8 b)
showExpr d (Neg a) = showParen (d > 9) (showString "neg " . showExpr 10 a)

commaSep :: [ShowS] -> ShowS
commaSep [] = id
commaSep [s] = s
commaSep (s : ss) = s . showChar ',' . commaSep ss

main :: IO ()
main = do
  print ("shows", shows (12 :: Int) "", shows (12 :: Int) "!", shows True " that", shows 'c' "", shows "s" "")
  print ("shows negative", shows (-5 :: Int) "", shows (-1.5 :: Double) "x", shows (Just (-5) :: Maybe Int) "")
  print ("shows = showsPrec 0", shows [1, 2 :: Int] "" == showsPrec 0 [1, 2 :: Int] "")
  print ("showChar", showChar 'a' "", showChar 'a' "bc", showChar '\n' "", showChar '"' "")
  print ("showChar = (:)", showChar 'x' "yz" == 'x' : "yz")
  print ("showString", showString "abc" "", showString "abc" "def", showString "" "x", showString "q\"q" "")
  print ("showString = (++)", showString "ab" "cd" == "ab" ++ "cd")
  print ("showString does not quote", showString "raw" "" == "raw", shows "raw" "" == "\"raw\"")
  print ("showParen False", showParen False (showString "x") "", showParen False (shows (1 :: Int)) "!")
  print ("showParen True", showParen True (showString "x") "", showParen True (shows (1 :: Int)) "!")
  print ("showParen nested", showParen True (showParen True (showChar 'x')) "", showParen True id "")
  print ("showParen by condition", map (\d -> showParen (d > 10) (showString "f x") "") [0, 10, 11 :: Int])
  print ("composition", (showString "a" . showChar 'b' . shows (3 :: Int) . showString "c") "")
  print ("composition with tail", (showChar '<' . shows (1 :: Int) . showChar '>') " rest")
  print ("commaSep", commaSep [] "", commaSep [shows (1 :: Int)] "", commaSep [shows (1 :: Int), shows 'x', showString "raw"] "")
  print ("foldr of ShowS", foldr (\x acc -> shows x . showChar ' ' . acc) id [1, 2, 3 :: Int] "end")
  print ("ShowS is String -> String", map (\f -> f "") ([shows (1 :: Int), showChar 'c', showString "s"] :: [ShowS]))
  putStrLn (showExpr 0 (Add (Lit 1) (Mul (Lit 2) (Lit 3))) "")
  putStrLn (showExpr 0 (Mul (Add (Lit 1) (Lit 2)) (Lit 3)) "")
  putStrLn (showExpr 0 (Add (Lit 1) (Add (Lit 2) (Lit 3))) "")
  putStrLn (showExpr 0 (Add (Add (Lit 1) (Lit 2)) (Lit 3)) "")
  putStrLn (showExpr 0 (Mul (Lit (-1)) (Neg (Lit (-2)))) "")
  putStrLn (showExpr 0 (Neg (Neg (Add (Lit 1) (Lit (-2))))) "")
  putStrLn (showExpr 11 (Add (Lit 1) (Lit 2)) " <- parenthesised")
  print ("lazy in continuation", take 3 (shows (123456 :: Int) undefined), take 2 (showString "ab" undefined), head (showChar 'h' undefined))
