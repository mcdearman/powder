-- Haskell 2010 Report 11.4: derived Show for a nullary constructor, a prefix one, one declared infix in
-- backquotes, and a record with an operator as a field name; each at precedence 0 and 11
module Main (main) where

data T
  = A
  | B Int Char
  | Int `C` Char
  | D {x :: Int, (%^) :: Char}
  deriving (Show)

main :: IO ()
main = do
  print A
  print (B 1 'a')
  print (C 2 'b')
  print (D 3 'c')
  putStrLn (showsPrec 11 A "")
  putStrLn (showsPrec 11 (B 1 'a') "")
  putStrLn (showsPrec 11 (C 2 'b') "")
  putStrLn (showsPrec 11 (D 3 'c') "")
  print [A, B (-1) 'z', (-2) `C` 'y', D {x = -3, (%^) = 'x'}]
  print (Just (C 4 'd'), x (D 5 'e'), (%^) (D 6 'f'))
