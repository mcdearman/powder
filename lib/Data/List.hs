module Data.List
  ( (++), head, last, tail, init, null, length, map, reverse, intersperse, intercalate, transpose,
    subsequences, permutations, foldl, foldl', foldl1, foldr, foldr1, concat, concatMap, and, or, any, all,
    sum, product, maximum, minimum, scanl, scanl1, scanr, scanr1, mapAccumL, mapAccumR,
    iterate, repeat, replicate, cycle, unfoldr, take, drop, splitAt, takeWhile, dropWhile, span, break,
    stripPrefix, group, inits, tails, isPrefixOf, isSuffixOf, isInfixOf, elem, notElem, lookup,
    find, filter, partition, (!!), elemIndex, elemIndices, findIndex, findIndices,
    zip, zip3, zip4, zip5, zip6, zip7, zipWith, zipWith3, zipWith4, zipWith5, zipWith6, zipWith7,
    unzip, unzip3, unzip4, unzip5, unzip6, unzip7, lines, words, unlines, unwords, foldl1',
    nub, delete, (\\), union, intersect, sort, insert, nubBy, deleteBy, deleteFirstsBy, unionBy,
    intersectBy, groupBy, sortBy, insertBy, maximumBy, minimumBy,
    genericLength, genericTake, genericDrop, genericSplitAt, genericIndex, genericReplicate
  )
where

import Powder.Base (Foldable (..), find, maximumBy, minimumBy)

infix 5 \\

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x : xs) = x : sep : intersperse sep xs

intercalate :: [a] -> [[a]] -> [a]
intercalate sep xss = concat (intersperse sep xss)

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : xss) = transpose xss
transpose ((x : xs) : xss) = (x : [h | h : _ <- xss]) : transpose (xs : [t | _ : t <- xss])

subsequences :: [a] -> [[a]]
subsequences xs = [] : nonEmpty xs
  where
    nonEmpty [] = []
    nonEmpty (y : ys) = [y] : foldr (\zs r -> zs : (y : zs) : r) [] (nonEmpty ys)

permutations :: [a] -> [[a]]
permutations xs0 = xs0 : perms xs0 []
  where
    perms [] _ = []
    perms (t : ts) is = foldr interleave (perms ts (t : is)) (permutations is)
      where
        interleave xs r = let (_, zs) = interleave' id xs r in zs
        interleave' _ [] r = (ts, r)
        interleave' f (y : ys) r = let (us, zs) = interleave' (f . (y :)) ys r in (y : us, f (t : y : us) : zs)

foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' f (x : xs) = foldl' f x xs
foldl1' _ [] = error "List.foldl1': empty list"

mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
mapAccumL _ s [] = (s, [])
mapAccumL f s (x : xs) =
  let (s', y) = f s x
      (s'', ys) = mapAccumL f s' xs
   in (s'', y : ys)

mapAccumR :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
mapAccumR _ s [] = (s, [])
mapAccumR f s (x : xs) =
  let (s', ys) = mapAccumR f s xs
      (s'', y) = f s' x
   in (s'', y : ys)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f b = case f b of
  Just (a, b') -> a : unfoldr f b'
  Nothing -> []

stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix [] ys = Just ys
stripPrefix (x : xs) (y : ys) | x == y = stripPrefix xs ys
stripPrefix _ _ = Nothing

group :: Eq a => [a] -> [[a]]
group = groupBy (==)

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x : xs) = [] : map (x :) (inits xs)

tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_ : rest) = xs : tails rest

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf xs ys = isPrefixOf (reverse xs) (reverse ys)

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = (filter p xs, filter (not . p) xs)

elemIndex :: Eq a => a -> [a] -> Maybe Int
elemIndex x = findIndex (== x)

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices x = findIndices (== x)

findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex p xs = case findIndices p xs of
  [] -> Nothing
  i : _ -> Just i

findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices p xs = [i | (x, i) <- zip xs [0 ..], p x]

nub :: Eq a => [a] -> [a]
nub = nubBy (==)

nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy _ [] = []
nubBy eq (x : xs) = x : nubBy eq (filter (\y -> not (eq x y)) xs)

delete :: Eq a => a -> [a] -> [a]
delete = deleteBy (==)

deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy _ _ [] = []
deleteBy eq x (y : ys) = if eq x y then ys else y : deleteBy eq x ys

(\\) :: Eq a => [a] -> [a] -> [a]
xs \\ ys = foldl (flip delete) xs ys

deleteFirstsBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
deleteFirstsBy eq = foldl (flip (deleteBy eq))

union :: Eq a => [a] -> [a] -> [a]
union = unionBy (==)

unionBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy eq xs ys = xs ++ foldl (flip (deleteBy eq)) (nubBy eq ys) xs

intersect :: Eq a => [a] -> [a] -> [a]
intersect = intersectBy (==)

intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy eq xs ys = [x | x <- xs, any (eq x) ys]

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy eq (x : xs) = let (same, rest) = span (eq x) xs in (x : same) : groupBy eq rest

sort :: Ord a => [a] -> [a]
sort = sortBy compare

-- A merge sort, which keeps the order of what compares equal.
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp = mergeAll . map (\x -> [x])
  where
    mergeAll [] = []
    mergeAll [xs] = xs
    mergeAll xss = mergeAll (pairs xss)
    pairs (xs : ys : rest) = merge xs ys : pairs rest
    pairs xss = xss
    merge [] ys = ys
    merge xs [] = xs
    merge xs@(x : xs') ys@(y : ys') = case cmp x y of
      GT -> y : merge xs ys'
      _ -> x : merge xs' ys

insert :: Ord a => a -> [a] -> [a]
insert = insertBy compare

insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _ x [] = [x]
insertBy cmp x ys@(y : ys') = case cmp x y of
  GT -> y : insertBy cmp x ys'
  _ -> x : ys

genericLength :: Num i => [a] -> i
genericLength [] = 0
genericLength (_ : xs) = 1 + genericLength xs

genericTake :: Integral i => i -> [a] -> [a]
genericTake n xs = take (fromIntegral n) xs

genericDrop :: Integral i => i -> [a] -> [a]
genericDrop n xs = drop (fromIntegral n) xs

genericSplitAt :: Integral i => i -> [a] -> ([a], [a])
genericSplitAt n xs = splitAt (fromIntegral n) xs

genericIndex :: Integral i => [a] -> i -> a
genericIndex xs n = xs !! fromIntegral n

genericReplicate :: Integral i => i -> a -> [a]
genericReplicate n x = replicate (fromIntegral n) x

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 = zipWith4 (\a b c d -> (a, b, c, d))

zipWith4 :: (a -> b -> c -> d -> r) -> [a] -> [b] -> [c] -> [d] -> [r]
zipWith4 z (a : as) (b : bs) (c : cs) (d : ds) = z a b c d : zipWith4 z as bs cs ds
zipWith4 _ _ _ _ _ = []

unzip4 :: [(a, b, c, d)] -> ([a], [b], [c], [d])
unzip4 = foldr (\(a, b, c, d) ~(as, bs, cs, ds) -> (a : as, b : bs, c : cs, d : ds)) ([], [], [], [])

zip5 :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a, b, c, d, e)]
zip5 = zipWith5 (\a b c d e -> (a, b, c, d, e))

zipWith5 :: (a -> b -> c -> d -> e -> r) -> [a] -> [b] -> [c] -> [d] -> [e] -> [r]
zipWith5 z (a : as) (b : bs) (c : cs) (d : ds) (e : es) = z a b c d e : zipWith5 z as bs cs ds es
zipWith5 _ _ _ _ _ _ = []

unzip5 :: [(a, b, c, d, e)] -> ([a], [b], [c], [d], [e])
unzip5 = foldr (\(a, b, c, d, e) ~(as, bs, cs, ds, es) -> (a : as, b : bs, c : cs, d : ds, e : es)) ([], [], [], [], [])

zip6 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a, b, c, d, e, f)]
zip6 = zipWith6 (\a b c d e f -> (a, b, c, d, e, f))

zipWith6 :: (a -> b -> c -> d -> e -> f -> r) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [r]
zipWith6 z (a : as) (b : bs) (c : cs) (d : ds) (e : es) (f : fs) = z a b c d e f : zipWith6 z as bs cs ds es fs
zipWith6 _ _ _ _ _ _ _ = []

unzip6 :: [(a, b, c, d, e, f)] -> ([a], [b], [c], [d], [e], [f])
unzip6 = foldr (\(a, b, c, d, e, f) ~(as, bs, cs, ds, es, fs) -> (a : as, b : bs, c : cs, d : ds, e : es, f : fs)) ([], [], [], [], [], [])

zip7 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [(a, b, c, d, e, f, g)]
zip7 = zipWith7 (\a b c d e f g -> (a, b, c, d, e, f, g))

zipWith7 :: (a -> b -> c -> d -> e -> f -> g -> r) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [r]
zipWith7 z (a : as) (b : bs) (c : cs) (d : ds) (e : es) (f : fs) (g : gs) = z a b c d e f g : zipWith7 z as bs cs ds es fs gs
zipWith7 _ _ _ _ _ _ _ _ = []

unzip7 :: [(a, b, c, d, e, f, g)] -> ([a], [b], [c], [d], [e], [f], [g])
unzip7 = foldr (\(a, b, c, d, e, f, g) ~(as, bs, cs, ds, es, fs, gs) -> (a : as, b : bs, c : cs, d : ds, e : es, f : fs, g : gs)) ([], [], [], [], [], [], [])
