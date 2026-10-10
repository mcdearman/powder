-- Haskell 2010 Report 3.17.3, 4.4.3.1: sixteen clauses over a string, a number and a Bool; the first that
-- matches, top to bottom and left to right, is taken
module Main (main) where

matrix :: String -> Integer -> Bool -> String
matrix "aa" 0 False = "1"
matrix "aa" 0 True = "2"
matrix "aa" 1 False = "3"
matrix "aa" 1 True = "4"
matrix "ab" 0 False = "5"
matrix "ab" 0 True = "6"
matrix "ab" 1 False = "7"
matrix "ab" 1 True = "8"
matrix "ba" 0 False = "9"
matrix "ba" 0 True = "10"
matrix "ba" 1 False = "11"
matrix "ba" 1 True = "12"
matrix "bb" 0 False = "13"
matrix "bb" 0 True = "14"
matrix "bb" 1 False = "15"
matrix "bb" 1 True = "16"
matrix _ _ _ = "none"

main :: IO ()
main = do
  putStrLn (matrix "ba" 1 True)
  putStrLn (unwords [matrix s n f | s <- ["aa", "ab", "ba", "bb"], n <- [0, 1], f <- [False, True]])
  putStrLn (unwords [matrix "a" 0 True, matrix "aaa" 0 True, matrix "bb" 2 False, matrix "" 1 True])
