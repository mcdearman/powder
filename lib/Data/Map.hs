{-# LANGUAGE GHC2021 #-}
-- Maps from keys that have an order to values: trees kept balanced by
-- their sizes, as the containers package has them. A map is lazy in its
-- values.
module Data.Map
  ( Map,
    empty,
    singleton,
    fromList,
    fromListWith,
    fromListWithKey,
    fromAscList,
    fromDistinctAscList,
    insert,
    insertWith,
    insertWithKey,
    insertLookupWithKey,
    delete,
    adjust,
    adjustWithKey,
    update,
    updateWithKey,
    alter,
    lookup,
    (!),
    (!?),
    findWithDefault,
    member,
    notMember,
    lookupLT,
    lookupGT,
    lookupLE,
    lookupGE,
    null,
    size,
    union,
    unionWith,
    unionWithKey,
    unions,
    unionsWith,
    difference,
    (\\),
    differenceWith,
    intersection,
    intersectionWith,
    intersectionWithKey,
    disjoint,
    restrictKeys,
    withoutKeys,
    map,
    mapWithKey,
    traverseWithKey,
    mapAccum,
    mapAccumWithKey,
    mapKeys,
    mapKeysWith,
    mapKeysMonotonic,
    foldr,
    foldl,
    foldrWithKey,
    foldlWithKey,
    foldMapWithKey,
    foldr',
    foldl',
    foldrWithKey',
    foldlWithKey',
    elems,
    keys,
    assocs,
    keysSet,
    toList,
    toAscList,
    toDescList,
    filter,
    filterWithKey,
    partition,
    partitionWithKey,
    mapMaybe,
    mapMaybeWithKey,
    mapEither,
    mapEitherWithKey,
    split,
    splitLookup,
    isSubmapOf,
    isSubmapOfBy,
    lookupIndex,
    findIndex,
    elemAt,
    deleteAt,
    take,
    drop,
    splitAt,
    lookupMin,
    lookupMax,
    findMin,
    findMax,
    deleteMin,
    deleteMax,
    deleteFindMin,
    deleteFindMax,
    minView,
    maxView,
    minViewWithKey,
    maxViewWithKey,
  )
where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Prelude hiding (drop, filter, foldl, foldr, lookup, map, null, splitAt, take)

-- A tree is nothing, or a key and its value between the keys below it
-- and those above, with how many keys it has in all.
data Map k a = Tip | Bin Int k a (Map k a) (Map k a)

infixl 9 !, !?, \\

empty :: Map k a
empty = Tip

singleton :: k -> a -> Map k a
singleton k x = Bin 1 k x Tip Tip

null :: Map k a -> Bool
null Tip = True
null _ = False

size :: Map k a -> Int
size Tip = 0
size (Bin n _ _ _ _) = n

lookup :: Ord k => k -> Map k a -> Maybe a
lookup _ Tip = Nothing
lookup k (Bin _ kx x l r) = case compare k kx of
  LT -> lookup k l
  GT -> lookup k r
  EQ -> Just x

member :: Ord k => k -> Map k a -> Bool
member k m = isJust (lookup k m)

notMember :: Ord k => k -> Map k a -> Bool
notMember k m = not (member k m)

findWithDefault :: Ord k => a -> k -> Map k a -> a
findWithDefault d k m = maybe d id (lookup k m)

(!) :: Ord k => Map k a -> k -> a
m ! k = case lookup k m of
  Just x -> x
  Nothing -> error "Map.!: given key is not an element in the map"

(!?) :: Ord k => Map k a -> k -> Maybe a
m !? k = lookup k m

lookupLT :: Ord k => k -> Map k a -> Maybe (k, a)
lookupLT k = go Nothing
  where
    go best Tip = best
    go best (Bin _ kx x l r)
      | kx < k = go (Just (kx, x)) r
      | otherwise = go best l

lookupGT :: Ord k => k -> Map k a -> Maybe (k, a)
lookupGT k = go Nothing
  where
    go best Tip = best
    go best (Bin _ kx x l r)
      | kx > k = go (Just (kx, x)) l
      | otherwise = go best r

lookupLE :: Ord k => k -> Map k a -> Maybe (k, a)
lookupLE k = go Nothing
  where
    go best Tip = best
    go best (Bin _ kx x l r) = case compare kx k of
      LT -> go (Just (kx, x)) r
      EQ -> Just (kx, x)
      GT -> go best l

lookupGE :: Ord k => k -> Map k a -> Maybe (k, a)
lookupGE k = go Nothing
  where
    go best Tip = best
    go best (Bin _ kx x l r) = case compare kx k of
      GT -> go (Just (kx, x)) l
      EQ -> Just (kx, x)
      LT -> go best r

-- Keeping a tree balanced: neither side more than three times the other.

bin :: k -> a -> Map k a -> Map k a -> Map k a
bin k x l r = Bin (size l + size r + 1) k x l r

balance :: k -> a -> Map k a -> Map k a -> Map k a
balance k x l r
  | sl + sr <= 1 = Bin (sl + sr + 1) k x l r
  | sr > 3 * sl = rotateL k x l r
  | sl > 3 * sr = rotateR k x l r
  | otherwise = Bin (sl + sr + 1) k x l r
  where
    sl = size l
    sr = size r

rotateL :: k -> a -> Map k a -> Map k a -> Map k a
rotateL k x l r@(Bin _ _ _ ly ry)
  | size ly < 2 * size ry = singleL k x l r
  | otherwise = doubleL k x l r
rotateL k x l Tip = bin k x l Tip

rotateR :: k -> a -> Map k a -> Map k a -> Map k a
rotateR k x l@(Bin _ _ _ ly ry) r
  | size ry < 2 * size ly = singleR k x l r
  | otherwise = doubleR k x l r
rotateR k x Tip r = bin k x Tip r

singleL, singleR, doubleL, doubleR :: k -> a -> Map k a -> Map k a -> Map k a
singleL k1 x1 t1 (Bin _ k2 x2 t2 t3) = bin k2 x2 (bin k1 x1 t1 t2) t3
singleL k1 x1 t1 Tip = bin k1 x1 t1 Tip
singleR k1 x1 (Bin _ k2 x2 t1 t2) t3 = bin k2 x2 t1 (bin k1 x1 t2 t3)
singleR k1 x1 Tip t3 = bin k1 x1 Tip t3
doubleL k1 x1 t1 (Bin _ k2 x2 (Bin _ k3 x3 t2 t3) t4) = bin k3 x3 (bin k1 x1 t1 t2) (bin k2 x2 t3 t4)
doubleL k1 x1 t1 t2 = singleL k1 x1 t1 t2
doubleR k1 x1 (Bin _ k2 x2 t1 (Bin _ k3 x3 t2 t3)) t4 = bin k3 x3 (bin k2 x2 t1 t2) (bin k1 x1 t3 t4)
doubleR k1 x1 t1 t2 = singleR k1 x1 t1 t2

insert :: Ord k => k -> a -> Map k a -> Map k a
insert k x Tip = singleton k x
insert k x (Bin n kx y l r) = case compare k kx of
  LT -> balance kx y (insert k x l) r
  GT -> balance kx y l (insert k x r)
  EQ -> Bin n k x l r

insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith f = insertWithKey (\_ new old -> f new old)

insertWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithKey _ k x Tip = singleton k x
insertWithKey f k x (Bin n kx y l r) = case compare k kx of
  LT -> balance kx y (insertWithKey f k x l) r
  GT -> balance kx y l (insertWithKey f k x r)
  EQ -> Bin n k (f k x y) l r

insertLookupWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> (Maybe a, Map k a)
insertLookupWithKey f k x m = (lookup k m, insertWithKey f k x m)

delete :: Ord k => k -> Map k a -> Map k a
delete _ Tip = Tip
delete k (Bin _ kx x l r) = case compare k kx of
  LT -> balance kx x (delete k l) r
  GT -> balance kx x l (delete k r)
  EQ -> glue l r

adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
adjust f = adjustWithKey (\_ x -> f x)

adjustWithKey :: Ord k => (k -> a -> a) -> k -> Map k a -> Map k a
adjustWithKey _ _ Tip = Tip
adjustWithKey f k (Bin n kx x l r) = case compare k kx of
  LT -> Bin n kx x (adjustWithKey f k l) r
  GT -> Bin n kx x l (adjustWithKey f k r)
  EQ -> Bin n kx (f kx x) l r

update :: Ord k => (a -> Maybe a) -> k -> Map k a -> Map k a
update f = updateWithKey (\_ x -> f x)

updateWithKey :: Ord k => (k -> a -> Maybe a) -> k -> Map k a -> Map k a
updateWithKey _ _ Tip = Tip
updateWithKey f k (Bin n kx x l r) = case compare k kx of
  LT -> balance kx x (updateWithKey f k l) r
  GT -> balance kx x l (updateWithKey f k r)
  EQ -> case f kx x of
    Just x' -> Bin n kx x' l r
    Nothing -> glue l r

alter :: Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
alter f k Tip = case f Nothing of
  Nothing -> Tip
  Just x -> singleton k x
alter f k (Bin n kx x l r) = case compare k kx of
  LT -> balance kx x (alter f k l) r
  GT -> balance kx x l (alter f k r)
  EQ -> case f (Just x) of
    Just x' -> Bin n kx x' l r
    Nothing -> glue l r

-- The least and the greatest.

lookupMin :: Map k a -> Maybe (k, a)
lookupMin Tip = Nothing
lookupMin (Bin _ k x Tip _) = Just (k, x)
lookupMin (Bin _ _ _ l _) = lookupMin l

lookupMax :: Map k a -> Maybe (k, a)
lookupMax Tip = Nothing
lookupMax (Bin _ k x _ Tip) = Just (k, x)
lookupMax (Bin _ _ _ _ r) = lookupMax r

findMin :: Map k a -> (k, a)
findMin m = maybe (error "Map.findMin: empty map has no minimal element") id (lookupMin m)

findMax :: Map k a -> (k, a)
findMax m = maybe (error "Map.findMax: empty map has no maximal element") id (lookupMax m)

deleteMin :: Map k a -> Map k a
deleteMin Tip = Tip
deleteMin (Bin _ _ _ Tip r) = r
deleteMin (Bin _ k x l r) = balance k x (deleteMin l) r

deleteMax :: Map k a -> Map k a
deleteMax Tip = Tip
deleteMax (Bin _ _ _ l Tip) = l
deleteMax (Bin _ k x l r) = balance k x l (deleteMax r)

deleteFindMin :: Map k a -> ((k, a), Map k a)
deleteFindMin m = case minViewWithKey m of
  Just found -> found
  Nothing -> (error "Map.deleteFindMin: can not return the minimal element of an empty map", Tip)

deleteFindMax :: Map k a -> ((k, a), Map k a)
deleteFindMax m = case maxViewWithKey m of
  Just found -> found
  Nothing -> (error "Map.deleteFindMax: can not return the maximal element of an empty map", Tip)

minViewWithKey :: Map k a -> Maybe ((k, a), Map k a)
minViewWithKey Tip = Nothing
minViewWithKey (Bin _ k x Tip r) = Just ((k, x), r)
minViewWithKey (Bin _ k x l r) = case minViewWithKey l of
  Just (least, l') -> Just (least, balance k x l' r)
  Nothing -> Nothing

maxViewWithKey :: Map k a -> Maybe ((k, a), Map k a)
maxViewWithKey Tip = Nothing
maxViewWithKey (Bin _ k x l Tip) = Just ((k, x), l)
maxViewWithKey (Bin _ k x l r) = case maxViewWithKey r of
  Just (greatest, r') -> Just (greatest, balance k x l r')
  Nothing -> Nothing

minView :: Map k a -> Maybe (a, Map k a)
minView m = fmap (\((_, x), rest) -> (x, rest)) (minViewWithKey m)

maxView :: Map k a -> Maybe (a, Map k a)
maxView m = fmap (\((_, x), rest) -> (x, rest)) (maxViewWithKey m)

-- Two trees as one: every key of the first below every key of the second.

glue :: Map k a -> Map k a -> Map k a
glue Tip r = r
glue l Tip = l
glue l r
  | size l > size r = case maxViewWithKey l of
      Just ((k, x), l') -> balance k x l' r
      Nothing -> r
  | otherwise = case minViewWithKey r of
      Just ((k, x), r') -> balance k x l r'
      Nothing -> l

-- The same, of any two sizes; and with a key and its value between.

merge :: Map k a -> Map k a -> Map k a
merge Tip r = r
merge l Tip = l
merge l@(Bin sl kx x lx rx) r@(Bin sr ky y ly ry)
  | 3 * sl < sr = balance ky y (merge l ly) ry
  | 3 * sr < sl = balance kx x lx (merge rx r)
  | otherwise = glue l r

link :: k -> a -> Map k a -> Map k a -> Map k a
link kx x Tip r = insertMin kx x r
link kx x l Tip = insertMax kx x l
link kx x l@(Bin sl ky y ly ry) r@(Bin sr kz z lz rz)
  | 3 * sl < sr = balance kz z (link kx x l lz) rz
  | 3 * sr < sl = balance ky y ly (link kx x ry r)
  | otherwise = bin kx x l r

insertMin, insertMax :: k -> a -> Map k a -> Map k a
insertMin kx x Tip = singleton kx x
insertMin kx x (Bin _ ky y l r) = balance ky y (insertMin kx x l) r
insertMax kx x Tip = singleton kx x
insertMax kx x (Bin _ ky y l r) = balance ky y l (insertMax kx x r)

split :: Ord k => k -> Map k a -> (Map k a, Map k a)
split k m = case splitLookup k m of
  (l, _, r) -> (l, r)

splitLookup :: Ord k => k -> Map k a -> (Map k a, Maybe a, Map k a)
splitLookup _ Tip = (Tip, Nothing, Tip)
splitLookup k (Bin _ kx x l r) = case compare k kx of
  LT -> case splitLookup k l of
    (below, found, above) -> (below, found, link kx x above r)
  GT -> case splitLookup k r of
    (below, found, above) -> (link kx x l below, found, above)
  EQ -> (l, Just x, r)

-- Two maps together. Where both have a key the first's value is kept.

union :: Ord k => Map k a -> Map k a -> Map k a
union = unionWith const

unionWith :: Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWith f = unionWithKey (\_ x y -> f x y)

unionWithKey :: Ord k => (k -> a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWithKey _ t1 Tip = t1
unionWithKey _ Tip t2 = t2
unionWithKey f (Bin _ k x l r) t2 = case splitLookup k t2 of
  (l2, found, r2) -> link k (maybe x (f k x) found) (unionWithKey f l l2) (unionWithKey f r r2)

unions :: Ord k => [Map k a] -> Map k a
unions = List.foldl' union empty

unionsWith :: Ord k => (a -> a -> a) -> [Map k a] -> Map k a
unionsWith f = List.foldl' (unionWith f) empty

difference :: Ord k => Map k a -> Map k b -> Map k a
difference Tip _ = Tip
difference t1 Tip = t1
difference t1 (Bin _ k _ l2 r2) = case split k t1 of
  (l1, r1) -> merge (difference l1 l2) (difference r1 r2)

(\\) :: Ord k => Map k a -> Map k b -> Map k a
m1 \\ m2 = difference m1 m2

differenceWith :: Ord k => (a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
differenceWith f t1 t2 = mapMaybeWithKey keep t1
  where
    keep k x = case lookup k t2 of
      Nothing -> Just x
      Just y -> f x y

intersection :: Ord k => Map k a -> Map k b -> Map k a
intersection = intersectionWith const

intersectionWith :: Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWith f = intersectionWithKey (\_ x y -> f x y)

intersectionWithKey :: Ord k => (k -> a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWithKey _ Tip _ = Tip
intersectionWithKey _ _ Tip = Tip
intersectionWithKey f (Bin _ k x l r) t2 = case splitLookup k t2 of
  (l2, found, r2) ->
    let l' = intersectionWithKey f l l2
        r' = intersectionWithKey f r r2
     in case found of
          Just y -> link k (f k x y) l' r'
          Nothing -> merge l' r'

disjoint :: Ord k => Map k a -> Map k b -> Bool
disjoint t1 t2 = null (intersection t1 t2)

restrictKeys :: Ord k => Map k a -> Set.Set k -> Map k a
restrictKeys m s = filterWithKey (\k _ -> Set.member k s) m

withoutKeys :: Ord k => Map k a -> Set.Set k -> Map k a
withoutKeys m s = filterWithKey (\k _ -> Set.notMember k s) m

isSubmapOf :: (Ord k, Eq a) => Map k a -> Map k a -> Bool
isSubmapOf = isSubmapOfBy (==)

isSubmapOfBy :: Ord k => (a -> b -> Bool) -> Map k a -> Map k b -> Bool
isSubmapOfBy f t1 t2 = all within (toAscList t1)
  where
    within (k, x) = maybe False (f x) (lookup k t2)

-- Over every value, and every key.

map :: (a -> b) -> Map k a -> Map k b
map f = mapWithKey (\_ x -> f x)

mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
mapWithKey _ Tip = Tip
mapWithKey f (Bin n k x l r) = Bin n k (f k x) (mapWithKey f l) (mapWithKey f r)

traverseWithKey :: Applicative t => (k -> a -> t b) -> Map k a -> t (Map k b)
traverseWithKey _ Tip = pure Tip
traverseWithKey f (Bin n k x l r) = (\l' x' r' -> Bin n k x' l' r') <$> traverseWithKey f l <*> f k x <*> traverseWithKey f r

mapAccum :: (a -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
mapAccum f = mapAccumWithKey (\a _ x -> f a x)

mapAccumWithKey :: (a -> k -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
mapAccumWithKey _ a Tip = (a, Tip)
mapAccumWithKey f a (Bin n k x l r) =
  let (a1, l') = mapAccumWithKey f a l
      (a2, x') = f a1 k x
      (a3, r') = mapAccumWithKey f a2 r
   in (a3, Bin n k x' l' r')

mapKeys :: Ord k2 => (k1 -> k2) -> Map k1 a -> Map k2 a
mapKeys f m = fromList [(f k, x) | (k, x) <- toAscList m]

mapKeysWith :: Ord k2 => (a -> a -> a) -> (k1 -> k2) -> Map k1 a -> Map k2 a
mapKeysWith c f m = fromListWith c [(f k, x) | (k, x) <- toAscList m]

mapKeysMonotonic :: (k1 -> k2) -> Map k1 a -> Map k2 a
mapKeysMonotonic _ Tip = Tip
mapKeysMonotonic f (Bin n k x l r) = Bin n (f k) x (mapKeysMonotonic f l) (mapKeysMonotonic f r)

foldr :: (a -> b -> b) -> b -> Map k a -> b
foldr f = foldrWithKey (\_ x z -> f x z)

foldl :: (b -> a -> b) -> b -> Map k a -> b
foldl f = foldlWithKey (\z _ x -> f z x)

foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey _ z Tip = z
foldrWithKey f z (Bin _ k x l r) = foldrWithKey f (f k x (foldrWithKey f z r)) l

foldlWithKey :: (b -> k -> a -> b) -> b -> Map k a -> b
foldlWithKey _ z Tip = z
foldlWithKey f z (Bin _ k x l r) = foldlWithKey f (f (foldlWithKey f z l) k x) r

foldMapWithKey :: Monoid m => (k -> a -> m) -> Map k a -> m
foldMapWithKey f = foldrWithKey (\k x rest -> f k x <> rest) mempty

foldr' :: (a -> b -> b) -> b -> Map k a -> b
foldr' f = foldrWithKey' (\_ x z -> f x z)

foldl' :: (b -> a -> b) -> b -> Map k a -> b
foldl' f = foldlWithKey' (\z _ x -> f z x)

foldrWithKey' :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey' _ z Tip = z
foldrWithKey' f z (Bin _ k x l r) = let z' = foldrWithKey' f z r in z' `seq` (let z'' = f k x z' in z'' `seq` foldrWithKey' f z'' l)

foldlWithKey' :: (b -> k -> a -> b) -> b -> Map k a -> b
foldlWithKey' _ z Tip = z
foldlWithKey' f z (Bin _ k x l r) = let z' = foldlWithKey' f z l in z' `seq` (let z'' = f z' k x in z'' `seq` foldlWithKey' f z'' r)

elems :: Map k a -> [a]
elems = foldr (:) []

keys :: Map k a -> [k]
keys = foldrWithKey (\k _ ks -> k : ks) []

keysSet :: Map k a -> Set.Set k
keysSet m = Set.fromDistinctAscList (keys m)

assocs :: Map k a -> [(k, a)]
assocs = toAscList

toList :: Map k a -> [(k, a)]
toList = toAscList

toAscList :: Map k a -> [(k, a)]
toAscList = foldrWithKey (\k x rest -> (k, x) : rest) []

toDescList :: Map k a -> [(k, a)]
toDescList = foldlWithKey (\rest k x -> (k, x) : rest) []

fromList :: Ord k => [(k, a)] -> Map k a
fromList = List.foldl' (\m (k, x) -> insert k x m) empty

fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> Map k a
fromListWith f = fromListWithKey (\_ x y -> f x y)

fromListWithKey :: Ord k => (k -> a -> a -> a) -> [(k, a)] -> Map k a
fromListWithKey f = List.foldl' (\m (k, x) -> insertWithKey f k x m) empty

fromAscList :: Eq k => [(k, a)] -> Map k a
fromAscList = fromDistinctAscList . latest
  where
    -- Of keys that are the same, the last one's value.
    latest ((k1, _) : rest@((k2, _) : _)) | k1 == k2 = latest rest
    latest (kx : rest) = kx : latest rest
    latest [] = []

-- From keys already in order, each once: the middle one at the top.
fromDistinctAscList :: [(k, a)] -> Map k a
fromDistinctAscList kxs = fst (build (length kxs) kxs)
  where
    build 0 rest = (Tip, rest)
    build n rest =
      let half = n `div` 2
          (l, after) = build half rest
       in case after of
            (k, x) : more -> let (r, end) = build (n - half - 1) more in (bin k x l r, end)
            [] -> (l, [])

-- Some of a map.

filter :: (a -> Bool) -> Map k a -> Map k a
filter p = filterWithKey (\_ x -> p x)

filterWithKey :: (k -> a -> Bool) -> Map k a -> Map k a
filterWithKey _ Tip = Tip
filterWithKey p (Bin _ k x l r)
  | p k x = link k x (filterWithKey p l) (filterWithKey p r)
  | otherwise = merge (filterWithKey p l) (filterWithKey p r)

partition :: (a -> Bool) -> Map k a -> (Map k a, Map k a)
partition p = partitionWithKey (\_ x -> p x)

partitionWithKey :: (k -> a -> Bool) -> Map k a -> (Map k a, Map k a)
partitionWithKey p m = (filterWithKey p m, filterWithKey (\k x -> not (p k x)) m)

mapMaybe :: (a -> Maybe b) -> Map k a -> Map k b
mapMaybe f = mapMaybeWithKey (\_ x -> f x)

mapMaybeWithKey :: (k -> a -> Maybe b) -> Map k a -> Map k b
mapMaybeWithKey _ Tip = Tip
mapMaybeWithKey f (Bin _ k x l r) = case f k x of
  Just y -> link k y (mapMaybeWithKey f l) (mapMaybeWithKey f r)
  Nothing -> merge (mapMaybeWithKey f l) (mapMaybeWithKey f r)

mapEither :: (a -> Either b c) -> Map k a -> (Map k b, Map k c)
mapEither f = mapEitherWithKey (\_ x -> f x)

mapEitherWithKey :: (k -> a -> Either b c) -> Map k a -> (Map k b, Map k c)
mapEitherWithKey f m = (mapMaybeWithKey (\k x -> either Just (const Nothing) (f k x)) m, mapMaybeWithKey (\k x -> either (const Nothing) Just (f k x)) m)

-- By place, counting keys from the least.

lookupIndex :: Ord k => k -> Map k a -> Maybe Int
lookupIndex = go 0
  where
    go _ _ Tip = Nothing
    go before k (Bin _ kx _ l r) = case compare k kx of
      LT -> go before k l
      GT -> go (before + size l + 1) k r
      EQ -> Just (before + size l)

findIndex :: Ord k => k -> Map k a -> Int
findIndex k m = maybe (error "Map.findIndex: element is not in the map") id (lookupIndex k m)

elemAt :: Int -> Map k a -> (k, a)
elemAt _ Tip = error "Map.elemAt: index out of range"
elemAt i (Bin _ k x l r) = case compare i (size l) of
  LT -> elemAt i l
  GT -> elemAt (i - size l - 1) r
  EQ -> (k, x)

deleteAt :: Int -> Map k a -> Map k a
deleteAt _ Tip = error "Map.deleteAt: index out of range"
deleteAt i (Bin _ k x l r) = case compare i (size l) of
  LT -> balance k x (deleteAt i l) r
  GT -> balance k x l (deleteAt (i - size l - 1) r)
  EQ -> glue l r

take :: Int -> Map k a -> Map k a
take n m = fst (splitAt n m)

drop :: Int -> Map k a -> Map k a
drop n m = snd (splitAt n m)

splitAt :: Int -> Map k a -> (Map k a, Map k a)
splitAt _ Tip = (Tip, Tip)
splitAt n m@(Bin _ k x l r)
  | n <= 0 = (Tip, m)
  | n >= size m = (m, Tip)
  | otherwise = case compare n (size l) of
      LT -> case splitAt n l of
        (first, rest) -> (first, link k x rest r)
      GT -> case splitAt (n - size l - 1) r of
        (first, rest) -> (link k x l first, rest)
      EQ -> (l, insertMin k x r)

instance (Eq k, Eq a) => Eq (Map k a) where
  m1 == m2 = size m1 == size m2 && toAscList m1 == toAscList m2

instance (Ord k, Ord a) => Ord (Map k a) where
  compare m1 m2 = compare (toAscList m1) (toAscList m2)

instance (Show k, Show a) => Show (Map k a) where
  showsPrec d m = showParen (d > 10) (showString "fromList " . shows (toAscList m))

instance Functor (Map k) where
  fmap = map

instance Foldable.Foldable (Map k) where
  foldr = foldr
  foldMap f = foldr (\x rest -> f x <> rest) mempty
  length = size
  null = null
  toList = elems

instance Traversable (Map k) where
  traverse f = traverseWithKey (\_ x -> f x)

instance Ord k => Semigroup (Map k a) where
  (<>) = union

instance Ord k => Monoid (Map k a) where
  mempty = empty
