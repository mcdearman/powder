-- Haskell 2010 Report 4.4.3.2: pattern bindings are matched lazily (as if under an implicit ~)
module Main (main) where

pairA :: Int
pairB :: String
(pairA, pairB) = (1, "one")

quotient, remainder :: Int
(quotient, remainder) = 17 `divMod` 5

-- never demanded, so the failing match is never performed
unusedHead :: Int
Just unusedHead = Nothing

firstElem :: Int
restElems :: [Int]
(firstElem : restElems) = [10, 20, 30]

whole :: [Int]
wholeHead :: Int
whole@(wholeHead : _) = [7, 8, 9]

-- mutually recursive pattern binding
evens, odds :: [Int]
(evens, odds) = (0 : map (+ 1) odds, map (+ 1) evens)

lazyLocal :: Int
lazyLocal =
  let (a, b) = undefined :: (Int, Int)
      [c] = [] :: [Int]
  in 42

partlyUsed :: Int
partlyUsed =
  let (x, y) = (1, undefined :: Int)
  in x

main :: IO ()
main = do
  print ("tuple", pairA, pairB)
  print ("divMod", quotient, remainder)
  print ("list", firstElem, restElems)
  print ("as-pattern", whole, wholeHead)
  print ("recursive", take 4 evens, take 4 odds)
  print ("unused failing bindings", lazyLocal)
  print ("undefined component unused", partlyUsed)
  let [p, q] = [1, 2 :: Int]
      (m, (n, o)) = ('m', (True, "o"))
  print ("local list", p + q)
  print ("local nested", m, n, o)
  let (big, small) = (maximum values, minimum values)
      values = [3, 1, 4, 1, 5 :: Int]
  print ("local with later binding", big, small)
