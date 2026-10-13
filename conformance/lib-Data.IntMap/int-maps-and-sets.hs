{-# LANGUAGE GHC2021 #-}
-- Data.IntMap and Data.IntSet: maps and sets of Int that branch on the bits of their keys, with keys below
-- nothing, far apart, close together and at the ends of what an Int can be; and Data.Bits, which they stand on
module Main (main) where

import Data.Bits
import qualified Data.Foldable as F
import qualified Data.IntMap as IM
import qualified Data.IntMap.Strict as IMS
import Data.IntMap (IntMap, (!), (!?))
import qualified Data.IntSet as IS
import Data.IntSet (IntSet)
import Data.List (foldl', sort)

keys :: [Int]
keys = [5, -3, 1024, 0, -1, 63, 64, 65, -64, -65, 4096, 7, maxBound, minBound, 1000000, -1000000, 127, 128, 2, 1]

small :: IntMap String
small = IM.fromList [(k, show k) | k <- keys]

set :: IntSet
set = IS.fromList keys

-- Many keys, of both signs, in no order.
scattered :: [Int]
scattered = [((i * 7919) `mod` 4001 - 2000) * (if even i then 1 else 37) | i <- [1 .. 3000]]

ascending :: Ord a => [a] -> Bool
ascending xs = and (zipWith (<) xs (drop 1 xs))

main :: IO ()
main = do
  -- The bits themselves.
  print ([x .&. y | x <- [12, -12], y <- [10, -10 :: Int]], [x .|. y | x <- [12, -12], y <- [10, -10 :: Int]], [xor x y | x <- [12, -12], y <- [10, -10 :: Int]])
  print (map complement [0, 1, -1, 255 :: Int], shiftL 1 10 :: Int, shiftL (-3) 4 :: Int, shiftR 1024 3 :: Int, shiftR (-1024) 3 :: Int, shiftR (-1) 70 :: Int)
  print (shiftL 1 63 :: Int, shiftL 1 64 :: Int, shiftL 3 62 :: Int, map popCount [0, 1, 255, -1, minBound, maxBound :: Int], testBit (5 :: Int) 0, testBit (5 :: Int) 1, testBit (-1 :: Int) 63)
  print (shiftL 1 100 :: Integer, (2 ^ 70 + 5) .&. (2 ^ 70 + 3) :: Integer, xor (2 ^ 65) (-1) :: Integer, shiftR (-(2 ^ 80)) 75 :: Integer, popCount (2 ^ 100 - 1 :: Integer), popCount (-8 :: Integer))
  print (bit 5 :: Int, setBit (0 :: Int) 63, clearBit (-1 :: Int) 0, complementBit (5 :: Int) 2, (.&.) (0xFF00 :: Integer) 0x0FF0, (.|.) (0xFF00 :: Integer) 0x0FF0)
  -- Maps.
  print small
  print (IM.size small, IM.null small, IM.null (IM.empty :: IntMap ()), IM.keys small == sort keys, IM.elems small)
  print (map (`IM.lookup` small) [5, -3, 6, minBound, maxBound, -2], small ! 1024, small !? 1025, IM.findWithDefault "?" 3 small, IM.member (-65) small, IM.notMember (-65) small)
  print (map (`IM.lookupLT` small) [minBound, -64, 0, 3, 65, maxBound], map (`IM.lookupGT` small) [minBound, -64, 0, 3, 65, maxBound])
  print (map (`IM.lookupLE` small) [minBound, -66, -2, 3, 66, maxBound], map (`IM.lookupGE` small) [minBound, -66, -2, 3, 66, maxBound])
  print (map (`IM.lookupLT` IM.fromList [(10, 'a'), (20, 'b')]) [-5, 10, 15, 25], map (`IM.lookupGT` IM.fromList [(-10, 'a'), (-20, 'b')]) [-25, -15, -10, 5])
  print (IM.insert 6 "six" small == IM.insert 6 "six" (IM.insert 6 "no" small), IM.toList (IM.insertWith (++) 5 "new-" (IM.fromList [(5, "five"), (-5, "minus")])), IM.insertLookupWithKey (\k a b -> show k ++ a ++ b) 7 "x" (IM.fromList [(7, "y")]))
  print (IM.delete 0 small == IM.fromList [(k, show k) | k <- keys, k /= 0], IM.toList (IM.delete 99 (IM.fromList [(1, 'a')])), IM.toList (IM.adjust succ 1 (IM.fromList [(1, 'a'), (-1, 'z')])), IM.toList (IM.update (const Nothing) (-1) (IM.fromList [(1, 'a'), (-1, 'z')])))
  print (IM.toList (IM.alter (fmap succ) 1 (IM.fromList [(1, 'a')])), IM.toList (IM.alter (const (Just 'n')) (-9) (IM.fromList [(1, 'a')])), IM.toList (IM.alter (const Nothing) 1 (IM.fromList [(1, 'a')])))
  let a = IM.fromList [(k, k) | k <- [-5 .. 5]]
      b = IM.fromList [(k, k * 100) | k <- [-2, 0, 2, 4, 6, 800, -800]]
  print (IM.union a b, IM.union b a, IM.unionWith (+) a b, IM.unionWithKey (\k x y -> k + x + y) a b)
  print (IM.difference a b, a IM.\\ b, IM.difference b a, IM.intersection a b, IM.intersectionWith (,) a b, IM.intersectionWithKey (\k x y -> k + x + y) b a)
  print (IM.unions [a, b, IM.singleton 9 9], IM.unionsWith (+) [a, a, b], IM.disjoint a b, IM.disjoint a (IM.singleton 77 ()), IM.differenceWith (\x y -> if x > 0 then Just (x + y) else Nothing) a b)
  print (IM.restrictKeys a (IS.fromList [-9, -2, 3]), IM.withoutKeys a (IS.fromList [-5 .. 3]), IM.isSubmapOf (IM.fromList [(2, 2)]) a, IM.isSubmapOf b a, IM.keysSet b)
  print (IM.map (* 2) b, IM.mapWithKey (+) b, fmap show b, IM.mapAccum (\acc x -> (acc + x, acc)) 0 b, IM.mapAccumWithKey (\acc k _ -> (acc ++ show k, length acc)) "" b)
  print (IM.mapKeys negate b, IM.mapKeysWith (+) abs b, IM.mapKeysMonotonic (+ 1) b)
  print (IM.foldr (:) [] b, IM.foldl (flip (:)) [] b, IM.foldrWithKey (\k x r -> (k, x) : r) [] b, IM.foldlWithKey (\r k x -> (k + x) : r) [] b, IM.foldMapWithKey (\k _ -> [k]) b)
  print (IM.foldr' (+) 0 b, IM.foldl' (+) 0 b, IM.foldrWithKey' (\k _ n -> k + n) 0 b, IM.foldlWithKey' (\n k _ -> n * 3 + k) 0 b)
  print (IM.assocs b == IM.toList b, IM.toAscList b, IM.toDescList b, IM.fromListWith (++) [(1, "a"), (-1, "b"), (1, "c")], IM.fromAscList [(-2, 'a'), (3, 'b')], IM.fromDistinctAscList [(1, 'x'), (2, 'y')])
  print (IM.filter even a, IM.filterWithKey (\k _ -> k < 0) a, IM.partition (> 0) a, IM.mapMaybe (\x -> if x < 0 then Just (show x) else Nothing) a, IM.mapEither (\x -> if x < 0 then Left x else Right (show x)) a)
  print (IM.split 0 a, IM.split 99 a, IM.splitLookup (-2) a, IM.splitLookup 77 b)
  print (IM.lookupMin small, IM.lookupMax small, IM.findMin b, IM.findMax b, IM.lookupMin (IM.empty :: IntMap ()), IM.lookupMin (IM.fromList [(3, 'c'), (9, 'd')]), IM.lookupMax (IM.fromList [(-3, 'c'), (-9, 'd')]))
  print (IM.deleteMin b, IM.deleteMax b, IM.deleteFindMin b, IM.deleteFindMax b, IM.minView b, IM.maxViewWithKey b, IM.minView (IM.empty :: IntMap ()))
  print (sum b, length b, F.toList b, maximum b, elem 200 b, traverse (\x -> if x > 100000 then Nothing else Just (x + 1)) b, IM.traverseWithKey (\k x -> if k == 0 then Left x else Right k) b)
  print (a == IM.fromList (reverse (IM.toList a)), a == b, compare a b, a <> b == IM.union a b, mconcat [b, a] == IM.union b a, (mempty :: IntMap ()), showsPrec 11 (IM.singleton 1 'x') "")
  let big = IM.fromList [(k, k * 2) | k <- scattered]
      evens = IM.filterWithKey (\k _ -> even k) big
      odds = IM.filterWithKey (\k _ -> odd k) big
      fewer = foldl' (flip IM.delete) big (take 1500 scattered)
  print (IM.size big, ascending (IM.keys big), all (\k -> IM.lookup k big == Just (k * 2)) scattered, IM.lookup 123456789 big, sum (IM.elems big))
  print (IM.size evens, IM.size odds, IM.union evens odds == big, IM.union odds evens == big, IM.null (IM.intersection evens odds), IM.difference big evens == odds, IM.intersection big odds == odds)
  print (IM.size fewer, ascending (IM.keys fewer), all (`IM.notMember` fewer) (take 1500 scattered), IM.foldlWithKey' (\n k x -> if x == 2 * k then n + 1 else n) (0 :: Int) fewer == IM.size fewer)
  print (IMS.insertWith (+) 3 1 (IMS.fromList [(3, 41), (-3, 0)]), IMS.adjust (* 2) (-3) (IMS.fromList [(-3, 21 :: Int)]))
  -- Sets.
  print set
  print (IS.size set, IS.null set, IS.null IS.empty, IS.toList set == sort keys, IS.member minBound set, IS.member 6 set, IS.notMember 6 set, IS.singleton (-1))
  print (map (`IS.lookupLT` set) [minBound, -64, 0, 3, 64, 66, 129, maxBound], map (`IS.lookupGT` set) [minBound, -66, -64, 0, 3, 63, 126, maxBound])
  print (map (`IS.lookupLE` set) [minBound, -66, -2, 3, 66, maxBound], map (`IS.lookupGE` set) [minBound, -66, -2, 3, 66, maxBound])
  let s = IS.fromList [-70 .. 70]
      t = IS.fromList [-200, -66, -64, -1, 0, 1, 63, 64, 500]
  print (IS.insert 71 s == IS.fromList [-70 .. 71], IS.delete 0 s == IS.fromList ([-70 .. -1] ++ [1 .. 70]), IS.delete 999 t, IS.insert 2 t)
  print (IS.union s t == IS.fromList ([-200, 500] ++ [-70 .. 70]), IS.union t s == IS.union s t, IS.unions [t, IS.singleton 7, IS.fromList [8, -8]])
  print (IS.difference t s, t IS.\\ s, IS.size (IS.difference s t), IS.intersection s t, IS.intersection t s, IS.intersection t (IS.fromList [1000 .. 1100]))
  print (IS.isSubsetOf (IS.fromList [-1, 63]) t, IS.isSubsetOf t s, IS.isProperSubsetOf t t, IS.isProperSubsetOf (IS.fromList [0]) t, IS.disjoint t (IS.fromList [2, 3]), IS.disjoint t s)
  print (IS.filter even t, IS.partition (< 0) t, IS.split 0 t, IS.split 2 t, IS.splitMember 64 t, IS.splitMember 65 t, IS.map (`div` 2) t, IS.map negate t)
  print (IS.foldr (:) [] t, IS.foldl (flip (:)) [] t, IS.foldr' (+) 0 t, IS.foldl' (\n k -> n * 2 + k) 0 t, IS.elems t, IS.toAscList t, IS.toDescList t)
  print (IS.findMin set, IS.findMax set, IS.findMin t, IS.findMax t, IS.findMin (IS.fromList [70, 3]), IS.findMax (IS.fromList [-70, -3]))
  print (IS.deleteMin t, IS.deleteMax t, IS.deleteFindMin t, IS.deleteFindMax t, IS.minView t, IS.maxView t, IS.minView IS.empty)
  print (IS.fromAscList [1, 1, 2], IS.fromDistinctAscList [-1, 0, 1], t == IS.fromList (IS.toDescList t), t == s, compare s t, t <> s == IS.union t s, (mempty :: IntSet), showsPrec 11 (IS.singleton 3) "")
  let bigSet = IS.fromList scattered
      evenSet = IS.filter even bigSet
      oddSet = IS.filter odd bigSet
      fewerSet = foldl' (flip IS.delete) bigSet (take 1500 scattered)
  print (IS.size bigSet, ascending (IS.toList bigSet), all (`IS.member` bigSet) scattered, IS.member 123456789 bigSet, sum (IS.toList bigSet), IS.size bigSet == IM.size big, IS.toList bigSet == IM.keys big)
  print (IS.size evenSet, IS.size oddSet, IS.union evenSet oddSet == bigSet, IS.null (IS.intersection evenSet oddSet), IS.difference bigSet evenSet == oddSet, IS.intersection bigSet oddSet == oddSet)
  print (IS.size fewerSet, ascending (IS.toList fewerSet), all (`IS.notMember` fewerSet) (take 1500 scattered), IS.toList fewerSet == IM.keys fewer)
