-- Haskell 2010 Report 9 (PreludeText): read of a tuple with a negative number, a character and a string; a
-- malformed tuple has no parse
module Main (main) where

value :: (Int, Char, String)
value = read "(-5, 'a', \"abc\")"

bad :: [((Int, Char), String)]
bad = readsPrec 0 "( 1 'a')"

main :: IO ()
main = do
  print value
  print bad
  print (reads "(1,'a') rest" :: [((Int, Char), String)])
  print (reads "(1,)" :: [((Int, Int), String)])
  print (reads "(1,2,3)" :: [((Int, Int), String)])
  print (read " ( 7 , ( True , \"x\" ) ) " :: (Int, (Bool, String)))
  print (read "((),[(1,'a'),(2,'b')])" :: ((), [(Int, Char)]))
