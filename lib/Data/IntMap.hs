{-# LANGUAGE GHC2021 #-}
-- Maps from Int, as the containers package has them: a tree that
-- branches on the bits of the keys, from the highest bit two keys differ
-- at down. Nothing is rebalanced, since where a key goes is its bits; a
-- lookup takes no more steps than a key has bits, and two maps are put
-- together by going down both at once.
module Data.IntMap
  ( IntMap, Key, empty, singleton, fromList, fromListWith, fromListWithKey, fromAscList, fromDistinctAscList,
    insert, insertWith, insertWithKey, insertLookupWithKey, delete, adjust, adjustWithKey, update, updateWithKey,
    alter, lookup, (!), (!?), findWithDefault, member, notMember, lookupLT, lookupGT, lookupLE, lookupGE, null,
    size, union, unionWith, unionWithKey, unions, unionsWith, difference, (\\), differenceWith, intersection,
    intersectionWith, intersectionWithKey, disjoint, restrictKeys, withoutKeys, map, mapWithKey, traverseWithKey,
    mapAccum, mapAccumWithKey, mapKeys, mapKeysWith, mapKeysMonotonic, foldr, foldl, foldrWithKey, foldlWithKey,
    foldMapWithKey, foldr', foldl', foldrWithKey', foldlWithKey', elems, keys, assocs, keysSet, toList, toAscList,
    toDescList, filter, filterWithKey, partition, partitionWithKey, mapMaybe, mapMaybeWithKey, mapEither,
    mapEitherWithKey, split, splitLookup, isSubmapOf, isSubmapOfBy, lookupMin, lookupMax, findMin, findMax,
    deleteMin, deleteMax, deleteFindMin, deleteFindMax, minView, maxView, minViewWithKey, maxViewWithKey
  )
where

import Data.Bits
import qualified Data.Foldable as Foldable
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import Prelude hiding (filter, foldl, foldr, lookup, map, null)

type Key = Int

-- What the keys under a branch start with, down to the bit they branch
-- at, the rest nothing.
type Prefix = Int

-- The place of the bit a branch is on: 63 for the sign, 0 for the last.
type Mask = Int

-- A branch has the keys with nothing at its bit on its left and the
-- others on its right. Neither side is ever empty.
data IntMap a = Bin Prefix Mask (IntMap a) (IntMap a) | Tip Key a | Nil

infixl 9 !, !?, \\

-- Of the bits.

zero :: Key -> Mask -> Bool
zero k m = (k `shiftR` m) .&. 1 == 0

prefixAt :: Key -> Mask -> Prefix
prefixAt k m = if m >= 63 then 0 else (k `shiftR` (m + 1)) `shiftL` (m + 1)

nomatch :: Key -> Prefix -> Mask -> Bool
nomatch k p m = prefixAt k m /= p

-- The highest bit two keys differ at.
branchMask :: Prefix -> Prefix -> Mask
branchMask p1 p2 = fromInteger (primBitsHighest (toInteger (p1 `xor` p2)))

-- Two trees whose keys start differently, as one.
link :: Prefix -> IntMap a -> Prefix -> IntMap a -> IntMap a
link p1 t1 p2 t2
  | zero p1 m = Bin p m t1 t2
  | otherwise = Bin p m t2 t1
  where
    m = branchMask p1 p2
    p = prefixAt p1 m

-- A branch, unless a side of it is empty.
bin :: Prefix -> Mask -> IntMap a -> IntMap a -> IntMap a
bin _ _ l Nil = l
bin _ _ Nil r = r
bin p m l r = Bin p m l r

empty :: IntMap a
empty = Nil

singleton :: Key -> a -> IntMap a
singleton = Tip

null :: IntMap a -> Bool
null Nil = True
null _ = False

size :: IntMap a -> Int
size = go 0
  where
    go n Nil = n
    go n (Tip _ _) = n + 1
    go n (Bin _ _ l r) = go (go n l) r

lookup :: Key -> IntMap a -> Maybe a
lookup k = go
  where
    go (Bin _ m l r) = if zero k m then go l else go r
    go (Tip kx x) = if k == kx then Just x else Nothing
    go Nil = Nothing

member :: Key -> IntMap a -> Bool
member k t = case lookup k t of
  Just _ -> True
  Nothing -> False

notMember :: Key -> IntMap a -> Bool
notMember k t = not (member k t)

findWithDefault :: a -> Key -> IntMap a -> a
findWithDefault d k t = maybe d id (lookup k t)

(!) :: IntMap a -> Key -> a
t ! k = case lookup k t of
  Just x -> x
  Nothing -> error ("IntMap.!: key " ++ show k ++ " is not an element of the map")

(!?) :: IntMap a -> Key -> Maybe a
t !? k = lookup k t

-- The greatest key of a tree no part of which is on the sign.
greatest, least :: IntMap a -> Maybe (Key, a)
greatest Nil = Nothing
greatest (Tip k x) = Just (k, x)
greatest (Bin _ _ _ r) = greatest r
least Nil = Nothing
least (Tip k x) = Just (k, x)
least (Bin _ _ l _) = least l

lookupLT :: Key -> IntMap a -> Maybe (Key, a)
lookupLT k t = case t of
  Bin _ 63 l r -> if k >= 0 then go r l else go Nil r
  _ -> go Nil t
  where
    go below (Bin p m l r)
      | nomatch k p m = if k < p then greatest below else greatest r
      | zero k m = go below l
      | otherwise = go l r
    go below (Tip ky y)
      | k <= ky = greatest below
      | otherwise = Just (ky, y)
    go below Nil = greatest below

lookupGT :: Key -> IntMap a -> Maybe (Key, a)
lookupGT k t = case t of
  Bin _ 63 l r -> if k >= 0 then go Nil l else go l r
  _ -> go Nil t
  where
    go above (Bin p m l r)
      | nomatch k p m = if k < p then least l else least above
      | zero k m = go r l
      | otherwise = go above r
    go above (Tip ky y)
      | k >= ky = least above
      | otherwise = Just (ky, y)
    go above Nil = least above

lookupLE :: Key -> IntMap a -> Maybe (Key, a)
lookupLE k t = case lookup k t of
  Just x -> Just (k, x)
  Nothing -> lookupLT k t

lookupGE :: Key -> IntMap a -> Maybe (Key, a)
lookupGE k t = case lookup k t of
  Just x -> Just (k, x)
  Nothing -> lookupGT k t

insert :: Key -> a -> IntMap a -> IntMap a
insert k x = insertWithKey (\_ new _ -> new) k x

insertWith :: (a -> a -> a) -> Key -> a -> IntMap a -> IntMap a
insertWith f = insertWithKey (\_ new old -> f new old)

insertWithKey :: (Key -> a -> a -> a) -> Key -> a -> IntMap a -> IntMap a
insertWithKey f k x t = case t of
  Bin p m l r
    | nomatch k p m -> link k (Tip k x) p t
    | zero k m -> Bin p m (insertWithKey f k x l) r
    | otherwise -> Bin p m l (insertWithKey f k x r)
  Tip ky y
    | k == ky -> Tip k (f k x y)
    | otherwise -> link k (Tip k x) ky t
  Nil -> Tip k x

insertLookupWithKey :: (Key -> a -> a -> a) -> Key -> a -> IntMap a -> (Maybe a, IntMap a)
insertLookupWithKey f k x t = (lookup k t, insertWithKey f k x t)

delete :: Key -> IntMap a -> IntMap a
delete k t = case t of
  Bin p m l r
    | nomatch k p m -> t
    | zero k m -> bin p m (delete k l) r
    | otherwise -> bin p m l (delete k r)
  Tip ky _
    | k == ky -> Nil
    | otherwise -> t
  Nil -> Nil

adjust :: (a -> a) -> Key -> IntMap a -> IntMap a
adjust f = adjustWithKey (\_ x -> f x)

adjustWithKey :: (Key -> a -> a) -> Key -> IntMap a -> IntMap a
adjustWithKey f = updateWithKey (\k x -> Just (f k x))

update :: (a -> Maybe a) -> Key -> IntMap a -> IntMap a
update f = updateWithKey (\_ x -> f x)

updateWithKey :: (Key -> a -> Maybe a) -> Key -> IntMap a -> IntMap a
updateWithKey f k t = case t of
  Bin p m l r
    | nomatch k p m -> t
    | zero k m -> bin p m (updateWithKey f k l) r
    | otherwise -> bin p m l (updateWithKey f k r)
  Tip ky y
    | k == ky -> maybe Nil (Tip ky) (f k y)
    | otherwise -> t
  Nil -> Nil

alter :: (Maybe a -> Maybe a) -> Key -> IntMap a -> IntMap a
alter f k t = case t of
  Bin p m l r
    | nomatch k p m -> maybe t (\x -> link k (Tip k x) p t) (f Nothing)
    | zero k m -> bin p m (alter f k l) r
    | otherwise -> bin p m l (alter f k r)
  Tip ky y
    | k == ky -> maybe Nil (Tip ky) (f (Just y))
    | otherwise -> maybe t (\x -> link k (Tip k x) ky t) (f Nothing)
  Nil -> maybe Nil (Tip k) (f Nothing)

-- Two maps together, going down both at once.

union :: IntMap a -> IntMap a -> IntMap a
union = unionWith const

unionWith :: (a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
unionWith f = unionWithKey (\_ x y -> f x y)

unionWithKey :: (Key -> a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
unionWithKey f = go
  where
    go t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
      -- The one that branches on the higher bit has the other under it,
      -- or beside it.
      | m1 > m2 = if nomatch p2 p1 m1 then link p1 t1 p2 t2 else if zero p2 m1 then Bin p1 m1 (go l1 t2) r1 else Bin p1 m1 l1 (go r1 t2)
      | m2 > m1 = if nomatch p1 p2 m2 then link p1 t1 p2 t2 else if zero p1 m2 then Bin p2 m2 (go t1 l2) r2 else Bin p2 m2 l2 (go t1 r2)
      | p1 == p2 = Bin p1 m1 (go l1 l2) (go r1 r2)
      | otherwise = link p1 t1 p2 t2
    go (Tip k x) t2 = insertWithKey f k x t2
    go t1 (Tip k y) = insertWithKey (\k' new old -> f k' old new) k y t1
    go Nil t2 = t2
    go t1 Nil = t1

unions :: [IntMap a] -> IntMap a
unions = List.foldl' union empty

unionsWith :: (a -> a -> a) -> [IntMap a] -> IntMap a
unionsWith f = List.foldl' (unionWith f) empty

difference :: IntMap a -> IntMap b -> IntMap a
difference t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | m1 > m2 = if nomatch p2 p1 m1 then t1 else if zero p2 m1 then bin p1 m1 (difference l1 t2) r1 else bin p1 m1 l1 (difference r1 t2)
  | m2 > m1 = if nomatch p1 p2 m2 then t1 else if zero p1 m2 then difference t1 l2 else difference t1 r2
  | p1 == p2 = bin p1 m1 (difference l1 l2) (difference r1 r2)
  | otherwise = t1
difference t1@(Tip k _) t2 = if member k t2 then Nil else t1
difference t1 (Tip k _) = delete k t1
difference Nil _ = Nil
difference t1 Nil = t1

(\\) :: IntMap a -> IntMap b -> IntMap a
m1 \\ m2 = difference m1 m2

differenceWith :: (a -> b -> Maybe a) -> IntMap a -> IntMap b -> IntMap a
differenceWith f t1 t2 = mapMaybeWithKey keep t1
  where
    keep k x = case lookup k t2 of
      Nothing -> Just x
      Just y -> f x y

intersection :: IntMap a -> IntMap b -> IntMap a
intersection = intersectionWith const

intersectionWith :: (a -> b -> c) -> IntMap a -> IntMap b -> IntMap c
intersectionWith f = intersectionWithKey (\_ x y -> f x y)

intersectionWithKey :: (Key -> a -> b -> c) -> IntMap a -> IntMap b -> IntMap c
intersectionWithKey f = go
  where
    go t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
      | m1 > m2 = if nomatch p2 p1 m1 then Nil else if zero p2 m1 then go l1 t2 else go r1 t2
      | m2 > m1 = if nomatch p1 p2 m2 then Nil else if zero p1 m2 then go t1 l2 else go t1 r2
      | p1 == p2 = bin p1 m1 (go l1 l2) (go r1 r2)
      | otherwise = Nil
    go (Tip k x) t2 = maybe Nil (Tip k . f k x) (lookup k t2)
    go t1 (Tip k y) = maybe Nil (\x -> Tip k (f k x y)) (lookup k t1)
    go Nil _ = Nil
    go _ Nil = Nil

disjoint :: IntMap a -> IntMap b -> Bool
disjoint t1 t2 = null (intersection t1 t2)

restrictKeys :: IntMap a -> IntSet.IntSet -> IntMap a
restrictKeys t s = filterWithKey (\k _ -> IntSet.member k s) t

withoutKeys :: IntMap a -> IntSet.IntSet -> IntMap a
withoutKeys t s = filterWithKey (\k _ -> IntSet.notMember k s) t

isSubmapOf :: Eq a => IntMap a -> IntMap a -> Bool
isSubmapOf = isSubmapOfBy (==)

isSubmapOfBy :: (a -> b -> Bool) -> IntMap a -> IntMap b -> Bool
isSubmapOfBy f t1 t2 = all (\(k, x) -> maybe False (f x) (lookup k t2)) (toAscList t1)

-- Over every value. A branch on the sign has the keys below nothing on
-- its right, so that side is gone through first.

map :: (a -> b) -> IntMap a -> IntMap b
map f = mapWithKey (\_ x -> f x)

mapWithKey :: (Key -> a -> b) -> IntMap a -> IntMap b
mapWithKey f = go
  where
    go (Bin p m l r) = Bin p m (go l) (go r)
    go (Tip k x) = Tip k (f k x)
    go Nil = Nil

traverseWithKey :: Applicative t => (Key -> a -> t b) -> IntMap a -> t (IntMap b)
traverseWithKey f = go
  where
    go (Bin p m l r)
      | m == 63 = (\r' l' -> Bin p m l' r') <$> go r <*> go l
      | otherwise = Bin p m <$> go l <*> go r
    go (Tip k x) = Tip k <$> f k x
    go Nil = pure Nil

mapAccum :: (a -> b -> (a, c)) -> a -> IntMap b -> (a, IntMap c)
mapAccum f = mapAccumWithKey (\a _ x -> f a x)

mapAccumWithKey :: (a -> Key -> b -> (a, c)) -> a -> IntMap b -> (a, IntMap c)
mapAccumWithKey f = go
  where
    go a (Bin p m l r)
      | m == 63 = let (a1, r') = go a r; (a2, l') = go a1 l in (a2, Bin p m l' r')
      | otherwise = let (a1, l') = go a l; (a2, r') = go a1 r in (a2, Bin p m l' r')
    go a (Tip k x) = let (a', x') = f a k x in (a', Tip k x')
    go a Nil = (a, Nil)

mapKeys :: (Key -> Key) -> IntMap a -> IntMap a
mapKeys f t = fromList [(f k, x) | (k, x) <- toAscList t]

mapKeysWith :: (a -> a -> a) -> (Key -> Key) -> IntMap a -> IntMap a
mapKeysWith c f t = fromListWith c [(f k, x) | (k, x) <- toAscList t]

mapKeysMonotonic :: (Key -> Key) -> IntMap a -> IntMap a
mapKeysMonotonic = mapKeys

foldr :: (a -> b -> b) -> b -> IntMap a -> b
foldr f = foldrWithKey (\_ x z -> f x z)

foldl :: (b -> a -> b) -> b -> IntMap a -> b
foldl f = foldlWithKey (\z _ x -> f z x)

foldrWithKey :: (Key -> a -> b -> b) -> b -> IntMap a -> b
foldrWithKey f z t = case t of
  Bin _ 63 l r -> go (go z l) r
  _ -> go z t
  where
    go z' Nil = z'
    go z' (Tip k x) = f k x z'
    go z' (Bin _ _ l r) = go (go z' r) l

foldlWithKey :: (b -> Key -> a -> b) -> b -> IntMap a -> b
foldlWithKey f z t = case t of
  Bin _ 63 l r -> go (go z r) l
  _ -> go z t
  where
    go z' Nil = z'
    go z' (Tip k x) = f z' k x
    go z' (Bin _ _ l r) = go (go z' l) r

foldMapWithKey :: Monoid m => (Key -> a -> m) -> IntMap a -> m
foldMapWithKey f = foldrWithKey (\k x rest -> f k x <> rest) mempty

foldr' :: (a -> b -> b) -> b -> IntMap a -> b
foldr' f = foldrWithKey' (\_ x z -> f x z)

foldl' :: (b -> a -> b) -> b -> IntMap a -> b
foldl' f = foldlWithKey' (\z _ x -> f z x)

foldrWithKey' :: (Key -> a -> b -> b) -> b -> IntMap a -> b
foldrWithKey' f = foldrWithKey (\k x z -> z `seq` f k x z)

foldlWithKey' :: (b -> Key -> a -> b) -> b -> IntMap a -> b
foldlWithKey' f z t = case t of
  Bin _ 63 l r -> go (go z r) l
  _ -> go z t
  where
    go z' Nil = z'
    go z' (Tip k x) = f z' k x
    go z' (Bin _ _ l r) = let z'' = go z' l in z'' `seq` go z'' r

elems :: IntMap a -> [a]
elems = foldr (:) []

keys :: IntMap a -> [Key]
keys = foldrWithKey (\k _ ks -> k : ks) []

keysSet :: IntMap a -> IntSet.IntSet
keysSet t = IntSet.fromDistinctAscList (keys t)

assocs :: IntMap a -> [(Key, a)]
assocs = toAscList

toList :: IntMap a -> [(Key, a)]
toList = toAscList

toAscList :: IntMap a -> [(Key, a)]
toAscList = foldrWithKey (\k x rest -> (k, x) : rest) []

toDescList :: IntMap a -> [(Key, a)]
toDescList = foldlWithKey (\rest k x -> (k, x) : rest) []

fromList :: [(Key, a)] -> IntMap a
fromList = List.foldl' (\t (k, x) -> insert k x t) Nil

fromListWith :: (a -> a -> a) -> [(Key, a)] -> IntMap a
fromListWith f = fromListWithKey (\_ x y -> f x y)

fromListWithKey :: (Key -> a -> a -> a) -> [(Key, a)] -> IntMap a
fromListWithKey f = List.foldl' (\t (k, x) -> insertWithKey f k x t) Nil

fromAscList :: [(Key, a)] -> IntMap a
fromAscList = fromList

fromDistinctAscList :: [(Key, a)] -> IntMap a
fromDistinctAscList = fromList

filter :: (a -> Bool) -> IntMap a -> IntMap a
filter p = filterWithKey (\_ x -> p x)

filterWithKey :: (Key -> a -> Bool) -> IntMap a -> IntMap a
filterWithKey p = go
  where
    go (Bin q m l r) = bin q m (go l) (go r)
    go t@(Tip k x) = if p k x then t else Nil
    go Nil = Nil

partition :: (a -> Bool) -> IntMap a -> (IntMap a, IntMap a)
partition p = partitionWithKey (\_ x -> p x)

partitionWithKey :: (Key -> a -> Bool) -> IntMap a -> (IntMap a, IntMap a)
partitionWithKey p t = (filterWithKey p t, filterWithKey (\k x -> not (p k x)) t)

mapMaybe :: (a -> Maybe b) -> IntMap a -> IntMap b
mapMaybe f = mapMaybeWithKey (\_ x -> f x)

mapMaybeWithKey :: (Key -> a -> Maybe b) -> IntMap a -> IntMap b
mapMaybeWithKey f = go
  where
    go (Bin p m l r) = bin p m (go l) (go r)
    go (Tip k x) = maybe Nil (Tip k) (f k x)
    go Nil = Nil

mapEither :: (a -> Either b c) -> IntMap a -> (IntMap b, IntMap c)
mapEither f = mapEitherWithKey (\_ x -> f x)

mapEitherWithKey :: (Key -> a -> Either b c) -> IntMap a -> (IntMap b, IntMap c)
mapEitherWithKey f t = (mapMaybeWithKey (\k x -> either Just (const Nothing) (f k x)) t, mapMaybeWithKey (\k x -> either (const Nothing) Just (f k x)) t)

split :: Key -> IntMap a -> (IntMap a, IntMap a)
split k t = (filterWithKey (\k' _ -> k' < k) t, filterWithKey (\k' _ -> k' > k) t)

splitLookup :: Key -> IntMap a -> (IntMap a, Maybe a, IntMap a)
splitLookup k t = case split k t of
  (below, above) -> (below, lookup k t, above)

-- The least and the greatest: below nothing is to the right of a branch
-- on the sign.

lookupMin :: IntMap a -> Maybe (Key, a)
lookupMin t = case t of
  Bin _ 63 _ r -> least r
  _ -> least t

lookupMax :: IntMap a -> Maybe (Key, a)
lookupMax t = case t of
  Bin _ 63 l _ -> greatest l
  _ -> greatest t

findMin :: IntMap a -> (Key, a)
findMin t = maybe (error "findMin: empty map has no minimal element") id (lookupMin t)

findMax :: IntMap a -> (Key, a)
findMax t = maybe (error "findMax: empty map has no maximal element") id (lookupMax t)

minViewWithKey :: IntMap a -> Maybe ((Key, a), IntMap a)
minViewWithKey t = fmap (\kx -> (kx, delete (fst kx) t)) (lookupMin t)

maxViewWithKey :: IntMap a -> Maybe ((Key, a), IntMap a)
maxViewWithKey t = fmap (\kx -> (kx, delete (fst kx) t)) (lookupMax t)

minView :: IntMap a -> Maybe (a, IntMap a)
minView t = fmap (\((_, x), rest) -> (x, rest)) (minViewWithKey t)

maxView :: IntMap a -> Maybe (a, IntMap a)
maxView t = fmap (\((_, x), rest) -> (x, rest)) (maxViewWithKey t)

deleteMin :: IntMap a -> IntMap a
deleteMin t = maybe Nil snd (minViewWithKey t)

deleteMax :: IntMap a -> IntMap a
deleteMax t = maybe Nil snd (maxViewWithKey t)

deleteFindMin :: IntMap a -> ((Key, a), IntMap a)
deleteFindMin t = maybe (error "deleteFindMin: empty map has no minimal element", Nil) id (minViewWithKey t)

deleteFindMax :: IntMap a -> ((Key, a), IntMap a)
deleteFindMax t = maybe (error "deleteFindMax: empty map has no maximal element", Nil) id (maxViewWithKey t)

instance Eq a => Eq (IntMap a) where
  t1 == t2 = toAscList t1 == toAscList t2

instance Ord a => Ord (IntMap a) where
  compare t1 t2 = compare (toAscList t1) (toAscList t2)

instance Show a => Show (IntMap a) where
  showsPrec d t = showParen (d > 10) (showString "fromList " . shows (toAscList t))

instance Functor IntMap where
  fmap = map

instance Foldable.Foldable IntMap where
  foldr = foldr
  foldMap f = foldr (\x rest -> f x <> rest) mempty
  length = size
  null = null
  toList = elems

instance Traversable IntMap where
  traverse f = traverseWithKey (\_ x -> f x)

instance Semigroup (IntMap a) where
  (<>) = union

instance Monoid (IntMap a) where
  mempty = empty
