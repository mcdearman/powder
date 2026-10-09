-- Haskell 2010 Report 3.14: do {e} = e, so a one-statement do needs no monad at all
module Main (main) where

five :: Int
five = do 5

greeting :: String
greeting = do { "hello" }

addOne :: Int -> Int
addOne = do (+ 1)

nestedDo :: Bool
nestedDo = do do do True

main :: IO ()
main = do
  print ("int", five)
  print ("string", greeting)
  print ("function", addOne 2)
  print ("nested", nestedDo)
  print ("tuple", do { (1 :: Int, 'c') })
  print ("arithmetic", (do 2) + (do 3 :: Int))
