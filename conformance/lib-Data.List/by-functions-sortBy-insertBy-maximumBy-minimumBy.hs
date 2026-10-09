-- Haskell 2010 Report 20.10.2 (Data.List): user-supplied comparison: sortBy (stable), insertBy,
-- maximumBy (the LAST of several maxima), minimumBy (the FIRST of several minima)
module Main (main) where

import Data.List

byFst :: (Int, Char) -> (Int, Char) -> Ordering
byFst (a, _) (b, _) = compare a b

byLength :: String -> String -> Ordering
byLength a b = compare (length a) (length b)

thenCmp :: Ordering -> Ordering -> Ordering
thenCmp EQ o = o
thenCmp o _ = o

ps :: [(Int, Char)]
ps = [(2, 'a'), (1, 'b'), (2, 'c'), (1, 'd'), (3, 'e'), (2, 'f'), (1, 'g')]

main :: IO ()
main = do
  print ("sortBy compare is sort", sortBy compare [3, 1, 2 :: Int])
  print ("sortBy descending", sortBy (flip compare) [3, 1, 4, 1, 5, 9, 2, 6 :: Int])
  print ("sortBy key is stable", sortBy byFst ps)
  print ("sortBy descending key is stable", sortBy (flip byFst) ps)
  print ("sortBy length is stable", sortBy byLength ["ccc", "a", "bb", "d", "ee", "fff", ""])
  print ("sortBy empty", sortBy byFst [])
  print ("sortBy singleton", sortBy byFst [(1, 'x')])
  print ("sortBy all equal keeps order", sortBy (\_ _ -> EQ) "keep this order")
  print ("sortBy two keys", sortBy (\(a, b) (c, d) -> compare b d `thenCmp` compare a c) ps)
  print ("sortBy already sorted", sortBy byFst [(1, 'a'), (1, 'b'), (2, 'c')])
  print ("insertBy", insertBy byFst (2, 'n') [(1, 'a'), (3, 'c')])
  print ("insertBy before equal keys", insertBy byFst (2, 'n') [(1, 'a'), (2, 'b'), (2, 'c'), (3, 'd')])
  print ("insertBy at end", insertBy byFst (9, 'n') [(1, 'a'), (3, 'c')])
  print ("insertBy at front", insertBy byFst (0, 'n') [(1, 'a'), (3, 'c')])
  print ("insertBy empty", insertBy byFst (0, 'n') [])
  print ("insertBy descending", insertBy (flip compare) 3 [5, 4, 2, 1 :: Int])
  print ("insertBy compare is insert", insertBy compare 3 [1, 5, 2 :: Int] == insert 3 [1, 5, 2])
  print ("insertBy length", insertBy byLength "xx" ["a", "bb", "ccc"])
  print ("maximumBy", maximumBy byFst [(1, 'a'), (3, 'b'), (2, 'c')])
  print ("maximumBy returns the last maximum", maximumBy byFst [(1, 'a'), (3, 'b'), (2, 'c'), (3, 'd')])
  print ("maximumBy all equal returns last", maximumBy byFst [(1, 'a'), (1, 'b'), (1, 'c')])
  print ("maximumBy singleton", maximumBy byFst [(5, 'z')])
  print ("maximumBy length", maximumBy byLength ["a", "ccc", "bb", "ddd", "e"])
  print ("maximumBy flipped comparison", maximumBy (flip compare) [3, 1, 2 :: Int])
  print ("maximumBy compare is maximum", maximumBy compare [3, 9, 2 :: Int] == maximum [3, 9, 2])
  print ("minimumBy", minimumBy byFst [(2, 'a'), (1, 'b'), (3, 'c')])
  print ("minimumBy returns the first minimum", minimumBy byFst [(2, 'a'), (1, 'b'), (3, 'c'), (1, 'd')])
  print ("minimumBy all equal returns first", minimumBy byFst [(1, 'a'), (1, 'b'), (1, 'c')])
  print ("minimumBy singleton", minimumBy byFst [(5, 'z')])
  print ("minimumBy length", minimumBy byLength ["bb", "a", "ccc", "d", "ee"])
  print ("minimumBy flipped comparison", minimumBy (flip compare) [3, 1, 2 :: Int])
  print ("minimumBy compare is minimum", minimumBy compare [3, 9, 2 :: Int] == minimum [3, 9, 2])
  print ("extremes agree with stable sort", maximumBy byFst ps == last (sortBy byFst ps), minimumBy byFst ps == head (sortBy byFst ps))
