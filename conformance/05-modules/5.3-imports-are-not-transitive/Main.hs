-- Haskell 2010 Report 5.3: importing A does not bring into scope what A itself imports (unless A re-exports it)
module Main (main) where

import A

-- neither B's names nor Data.Char's are visible through A, so these local definitions are unambiguous
secret :: String
secret = "Main.secret"

toUpper :: Char -> Char
toUpper c = c

ord :: Char -> Int
ord _ = 0

main :: IO ()
main = do
  print ("from A", revealed)
  print ("local names that A imports from elsewhere", secret, toUpper 'a', ord 'a')
