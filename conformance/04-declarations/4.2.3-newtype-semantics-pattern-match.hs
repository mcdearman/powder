-- Haskell 2010 Report 4.2.3: matching a newtype constructor pattern never forces the value (N _|_ = _|_)
module Main (main) where

newtype N = N Int

newtype Wrap a = Wrap a

newtype Age = Age { getAge :: Int }

matchN :: N -> Int
matchN (N _) = 42

matchWrap2 :: Wrap (Wrap a) -> Char
matchWrap2 (Wrap (Wrap _)) = 'w'

firstOfWrapped :: Wrap (a, b) -> a
firstOfWrapped (Wrap p) = fst p

main :: IO ()
main = do
  print ("function on bottom", matchN undefined)
  print ("function on N bottom", matchN (N undefined))
  print ("case on bottom", case undefined of N _ -> (1 :: Int))
  print ("case on N bottom", case N undefined of N _ -> (2 :: Int))
  print ("lambda on bottom", (\(N _) -> True) undefined)
  print ("let pattern", let N _ = undefined in (5 :: Int))
  print ("nested newtypes on bottom", matchWrap2 undefined)
  print ("wrapped lazy pair", firstOfWrapped (Wrap ('a', undefined :: Int)))
  print ("field label on age", getAge (Age 3))
  print ("selector after constructor", (getAge . Age) 7)
  print ("constructor is a function", sum (map (matchN . N) [1, 2, 3]))
  print ("unwrap", case Wrap 'x' of Wrap c -> c)
