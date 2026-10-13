{-# LANGUAGE GHC2021 #-}
-- Data.Map: maps from keys that have an order, as the containers package has them
module Main (main) where

import Data.Char (toUpper)
import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Map.Strict as Strict
import Data.Map (Map, (!), (!?))
import qualified Data.Set as Set

small :: Map Int String
small = Map.fromList [(5, "five"), (1, "one"), (3, "three"), (1, "uno"), (9, "nine")]

-- Many keys, put in in an order that is not theirs.
big :: Map Int Int
big = Map.fromList [(k, k * k) | i <- [1 .. 2000], let k = (i * 7919) `mod` 2003]

-- Whether a list is in order, each once.
ascending :: Ord a => [a] -> Bool
ascending xs = and (zipWith (<) xs (drop 1 xs))

main :: IO ()
main = do
  print small
  print (Map.empty :: Map Int Char, Map.singleton 'k' True, Map.null small, Map.null (Map.empty :: Map Int Int), Map.size small)
  print (Map.lookup 3 small, Map.lookup 4 small, small ! 5, small !? 9, small !? 2, Map.findWithDefault "none" 7 small)
  print (Map.member 1 small, Map.notMember 1 small, Map.member 2 small)
  print (Map.lookupLT 3 small, Map.lookupGT 3 small, Map.lookupLE 4 small, Map.lookupGE 4 small, Map.lookupLT 1 small, Map.lookupGE 10 small)
  print (Map.insert 4 "four" small, Map.insert 5 "FIVE" small)
  print (Map.insertWith (++) 5 "new-" small, Map.insertWithKey (\k new old -> show k ++ new ++ old) 3 "+" small)
  print (Map.insertLookupWithKey (\_ a b -> a ++ b) 9 "x" small, Map.insertLookupWithKey (\_ a b -> a ++ b) 8 "x" small)
  print (Map.delete 3 small, Map.delete 4 small)
  print (Map.adjust (map toUpper) 1 small, Map.adjustWithKey (\k v -> v ++ show k) 9 small, Map.adjust (map toUpper) 2 small)
  print (Map.update (\v -> if v == "one" then Nothing else Just (v ++ "!")) 1 small, Map.update (const Nothing) 5 small)
  print (Map.alter (fmap (++ "?")) 3 small, Map.alter (const (Just "new")) 4 small, Map.alter (const Nothing) 9 small, Map.alter (const Nothing) 8 small)
  let other = Map.fromList [(3, "THREE"), (4, "FOUR"), (9, "NINE")]
  print (Map.union small other, Map.union other small)
  print (Map.unionWith (\a b -> a ++ "/" ++ b) small other, Map.unionWithKey (\k a b -> show k ++ a ++ b) small other)
  print (Map.unions [small, other, Map.singleton 0 "zero"], Map.unionsWith (++) [small, other, small])
  print (Map.difference small other, small Map.\\ other, Map.differenceWith (\a b -> if length a > 4 then Just (a ++ b) else Nothing) small other)
  print (Map.intersection small other, Map.intersectionWith (,) small other, Map.intersectionWithKey (\k a b -> (k, length a + length b)) small other)
  print (Map.disjoint small other, Map.disjoint small (Map.singleton 2 ()), Map.restrictKeys small (Set.fromList [1, 2, 3]), Map.withoutKeys small (Set.fromList [1, 2, 3]))
  print (Map.isSubmapOf (Map.fromList [(1, "one")]) small, Map.isSubmapOf (Map.fromList [(1, "uno")]) small, Map.isSubmapOfBy (\a b -> length a <= length b) (Map.fromList [(1, "un")]) small)
  print (Map.map length small, Map.mapWithKey (\k v -> replicate k (head v)) small, fmap (take 2) small)
  print (Map.mapAccum (\acc v -> (acc + length v, reverse v)) 0 small, Map.mapAccumWithKey (\acc k v -> (acc ++ show k, length v)) "" small)
  print (Map.mapKeys (* 2) small, Map.mapKeys (`div` 4) small, Map.mapKeysWith (++) (`div` 4) small, Map.mapKeysMonotonic (+ 100) small)
  print (Map.foldr (\v acc -> v ++ "," ++ acc) "" small, Map.foldl (\acc v -> acc ++ "," ++ v) "" small)
  print (Map.foldrWithKey (\k v acc -> (k, length v) : acc) [] small, Map.foldlWithKey (\acc k v -> acc + k * length v) 0 small)
  print (Map.foldMapWithKey (\k v -> [show k ++ v]) small, Map.foldr' (+) 0 big, Map.foldl' (+) 0 big, Map.foldrWithKey' (\k _ n -> k + n) 0 big, Map.foldlWithKey' (\n k _ -> k + n) 0 big)
  print (Map.elems small, Map.keys small, Map.assocs small == Map.toList small, Map.keysSet small)
  print (Map.toAscList small, Map.toDescList small)
  print (Map.fromListWith (++) [(1, "a"), (2, "b"), (1, "c")], Map.fromListWithKey (\k a b -> show k ++ a ++ b) [(1, "a"), (2, "b"), (1, "c")])
  print (Map.fromAscList [(1, 'a'), (2, 'b'), (2, 'c'), (3, 'd')], Map.fromDistinctAscList (zip [1 :: Int ..] "abcdefg"))
  print (Map.filter ((> 3) . length) small, Map.filterWithKey (\k _ -> odd k && k > 2) small)
  print (Map.partition ((> 3) . length) small, Map.partitionWithKey (\k v -> k == length v) small)
  print (Map.mapMaybe (\v -> if length v > 3 then Just (length v) else Nothing) small, Map.mapMaybeWithKey (\k v -> if even k then Nothing else Just (k, v)) small)
  print (Map.mapEither (\v -> if length v > 3 then Left v else Right (length v)) small, Map.mapEitherWithKey (\k v -> if k < 4 then Left k else Right v) small)
  print (Map.split 3 small, Map.split 4 small, Map.splitLookup 5 small, Map.splitLookup 0 small)
  print (Map.lookupIndex 5 small, Map.lookupIndex 4 small, Map.findIndex 9 small, Map.elemAt 0 small, Map.elemAt 3 small, Map.deleteAt 1 small)
  print (Map.take 2 small, Map.drop 2 small, Map.splitAt 1 small, Map.take 0 small, Map.drop 9 small)
  print (Map.lookupMin small, Map.lookupMax small, Map.findMin small, Map.findMax small, Map.lookupMin (Map.empty :: Map Int Int))
  print (Map.deleteMin small, Map.deleteMax small, Map.deleteFindMin small, Map.deleteFindMax small)
  print (Map.minView small, Map.maxView small, Map.minViewWithKey small, Map.maxViewWithKey small, Map.minView (Map.empty :: Map Int Int))
  -- As a thing that is folded, mapped and gone through.
  print (sum (Map.map length small), length small, null small, F.toList small, maximum small, elem "nine" small, concat small)
  print (traverse (\v -> if null v then Nothing else Just (head v)) small, Map.traverseWithKey (\k v -> if k > 5 then Left (k, v) else Right (length v)) small)
  mapM_ print (Map.toList (Map.map length small))
  print (small == Map.fromList (reverse (Map.toList small)), small == other, compare small other, small < Map.insert 0 "zero" small, max small other == other)
  print (small <> other, mconcat [other, small, Map.singleton 7 "seven"], (mempty :: Map Int Int))
  print (showsPrec 11 small "", show (Map.fromList [(Just 'a', [1.5 :: Double]), (Nothing, [])]))
  -- With many keys: all there, in order, and the same however it is come by.
  print (Map.size big, ascending (Map.keys big), Map.lookup 1000 big, Map.lookup 2003 big, sum (Map.elems big) `mod` 1000003)
  let evens = Map.filterWithKey (\k _ -> even k) big
      odds = Map.filterWithKey (\k _ -> odd k) big
      fewer = foldr Map.delete big [0, 3 .. 2003]
  print (Map.size evens, Map.size odds, Map.union evens odds == big, Map.intersection evens odds, Map.size (Map.difference big evens), ascending (Map.keys (Map.union odds evens)))
  print (Map.size fewer, ascending (Map.keys fewer), Map.lookup 3 fewer, Map.lookup 4 fewer, Map.elemAt 100 fewer, Map.lookupIndex 1000 fewer)
  print (Map.foldlWithKey (\n k v -> if v == k * k then n + 1 else n) (0 :: Int) (Map.unionWith (+) evens (Map.map (* 0) odds)))
  -- Under its other name.
  print (Strict.insertWith (+) 'a' 1 (Strict.fromList [('a', 41), ('b', 0)]), Strict.adjust (* 2) 'b' (Strict.fromList [('b', 21 :: Int)]))
