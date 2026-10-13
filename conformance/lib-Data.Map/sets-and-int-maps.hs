{-# LANGUAGE GHC2021 #-}
-- Data.Set, and Data.IntMap and Data.IntSet: sets of things that have an order, and maps and sets of Int
module Main (main) where

import qualified Data.Foldable as F
import qualified Data.IntMap as IntMap
import qualified Data.IntMap.Strict as IntMapStrict
import Data.IntMap (IntMap)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Data.Set as Set
import Data.Set (Set)

small :: Set Char
small = Set.fromList "mississippi"

big :: Set Int
big = Set.fromList [(i * 7919) `mod` 2003 | i <- [1 .. 2000]]

ascending :: Ord a => [a] -> Bool
ascending xs = and (zipWith (<) xs (drop 1 xs))

main :: IO ()
main = do
  print small
  print (Set.empty :: Set Int, Set.singleton 'x', Set.null small, Set.null (Set.empty :: Set ()), Set.size small)
  print (Set.member 's' small, Set.notMember 's' small, Set.member 'z' small)
  print (Set.lookupLT 'm' small, Set.lookupGT 'm' small, Set.lookupLE 'n' small, Set.lookupGE 'n' small, Set.lookupLT 'a' small, Set.lookupGE 't' small)
  print (Set.insert 'a' small, Set.insert 's' small, Set.delete 's' small, Set.delete 'z' small)
  let other = Set.fromList "spam"
  print (Set.union small other, Set.unions [small, other, Set.singleton 'z'], Set.difference small other, small Set.\\ other, Set.intersection small other)
  print (Set.isSubsetOf (Set.fromList "sip") small, Set.isSubsetOf other small, Set.isProperSubsetOf small small, Set.isProperSubsetOf (Set.fromList "ms") small, Set.disjoint small other, Set.disjoint small (Set.fromList "xyz"))
  print (Set.filter (> 'm') small, Set.partition (> 'm') small, Set.split 'n' small, Set.splitMember 'p' small, Set.splitMember 'a' small)
  print (Set.map succ small, Set.map (`elem` "aeiou") small, Set.mapMonotonic fromEnum small)
  print (Set.foldr (:) [] small, Set.foldl (flip (:)) [] small, Set.foldr' (\c n -> n + fromEnum c) 0 small, Set.foldl' (\n c -> n * 2 + fromEnum c) 0 small)
  print (Set.lookupMin small, Set.lookupMax small, Set.findMin small, Set.findMax small, Set.lookupMin (Set.empty :: Set Int))
  print (Set.deleteMin small, Set.deleteMax small, Set.deleteFindMin small, Set.deleteFindMax small, Set.minView small, Set.maxView small, Set.minView (Set.empty :: Set Int))
  print (Set.elems small, Set.toList small, Set.toAscList small, Set.toDescList small)
  print (Set.fromAscList "aabccd", Set.fromDistinctAscList [1 .. 9 :: Int])
  print (Set.lookupIndex 'p' small, Set.lookupIndex 'q' small, Set.findIndex 's' small, Set.elemAt 0 small, Set.elemAt 3 small, Set.deleteAt 1 small)
  print (Set.take 2 small, Set.drop 2 small, Set.splitAt 1 small, Set.take 0 small, Set.drop 9 small)
  print (length small, sum (Set.fromList [3, 1, 2, 3 :: Int]), F.toList small, maximum small, elem 'p' small, null small, concatMap show (Set.fromList [3, 1, 2 :: Int]))
  print (small == Set.fromList "imps", small == other, compare small other, small <> other, mconcat [other, small], (mempty :: Set Int))
  print (showsPrec 11 small "", Set.fromList [Just [1 :: Int], Nothing, Just []])
  print (Set.size big, ascending (Set.toList big), Set.member 1000 big, Set.member 2003 big, sum (Set.toList big))
  let evens = Set.filter even big
      odds = Set.filter odd big
      fewer = foldr Set.delete big [0, 3 .. 2003]
  print (Set.size evens, Set.size odds, Set.union evens odds == big, Set.intersection evens odds, Set.size (Set.difference big evens), ascending (Set.toList (Set.union odds evens)))
  print (Set.size fewer, ascending (Set.toList fewer), Set.member 3 fewer, Set.member 4 fewer, Set.elemAt 100 fewer, Set.lookupIndex 1000 fewer)
  -- Maps and sets of Int.
  let m = IntMap.fromList [(5, "five"), (1, "one"), (3, "three")] :: IntMap String
  print (m, IntMap.lookup 3 m, IntMap.findWithDefault "?" 4 m, IntMap.insertWith (++) 3 "more-" m, IntMap.delete 1 m, IntMap.keys m, IntMap.elems m)
  print (IntMap.unionWith (++) m (IntMap.fromList [(3, "!"), (4, "four")]), IntMap.toList (IntMap.map length m), IntMap.member 5 m, IntMap.size m, IntMap.findMin m)
  print (IntMapStrict.foldrWithKey (\k v n -> k + length v + n) 0 m, IntMapStrict.adjust reverse 5 m, fmap length m, sum (fmap length m))
  let s = IntSet.fromList [5, 1, 3, 1, 4] :: IntSet
  print (s, IntSet.member 3 s, IntSet.insert 2 s, IntSet.delete 5 s, IntSet.toList s, IntSet.size s, IntSet.union s (IntSet.fromList [9, 0]), IntSet.findMax s)
