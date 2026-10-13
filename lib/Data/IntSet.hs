{-# LANGUAGE GHC2021 #-}
-- Sets of Int, as the containers package has them: a tree that branches
-- on the bits of its elements, with at each leaf sixty-four of them that
-- differ only in their last six bits, kept as the bits of one word. A
-- set of numbers close together is a few words.
module Data.IntSet
  ( IntSet, Key, empty, singleton, fromList, fromAscList, fromDistinctAscList, insert, delete, member, notMember,
    lookupLT, lookupGT, lookupLE, lookupGE, null, size, isSubsetOf, isProperSubsetOf, disjoint, union, unions,
    difference, (\\), intersection, filter, partition, split, splitMember, map, foldr, foldl, foldr', foldl',
    lookupMin, lookupMax, findMin, findMax, deleteMin, deleteMax, deleteFindMin, deleteFindMax, minView, maxView,
    elems, toList, toAscList, toDescList
  )
where

import Data.Bits
import qualified Data.List as List
import Prelude hiding (filter, foldl, foldr, map, null)

type Key = Int

-- What the elements under a branch start with, and the place of the bit
-- the branch is on: 63 for the sign, and never below 6.
type Prefix = Int

type Mask = Int

-- Which of the sixty-four elements a leaf could have it has: a bit each.
type BitMap = Int

data IntSet = Bin Prefix Mask IntSet IntSet | Tip Prefix BitMap | Nil

infixl 9 \\

-- Of the bits.

zero :: Key -> Mask -> Bool
zero k m = (k `shiftR` m) .&. 1 == 0

prefixAt :: Key -> Mask -> Prefix
prefixAt k m = if m >= 63 then 0 else (k `shiftR` (m + 1)) `shiftL` (m + 1)

nomatch :: Key -> Prefix -> Mask -> Bool
nomatch k p m = prefixAt k m /= p

branchMask :: Prefix -> Prefix -> Mask
branchMask p1 p2 = highest (p1 `xor` p2)

-- An element without its last six bits, and those as a bit of a word.
prefixOf :: Key -> Prefix
prefixOf k = (k `shiftR` 6) `shiftL` 6

bitOf :: Key -> BitMap
bitOf k = 1 `shiftL` (k .&. 63)

-- The places of the lowest and the highest bits of a word that has some.
lowest, highest :: BitMap -> Int
lowest b = fromInteger (primBitsLowest (toInteger b))
highest b = fromInteger (primBitsHighest (toInteger b))

count :: BitMap -> Int
count b = fromInteger (primBitsCount (toInteger b))

link :: Prefix -> IntSet -> Prefix -> IntSet -> IntSet
link p1 t1 p2 t2
  | zero p1 m = Bin p m t1 t2
  | otherwise = Bin p m t2 t1
  where
    m = branchMask p1 p2
    p = prefixAt p1 m

bin :: Prefix -> Mask -> IntSet -> IntSet -> IntSet
bin _ _ l Nil = l
bin _ _ Nil r = r
bin p m l r = Bin p m l r

tip :: Prefix -> BitMap -> IntSet
tip _ 0 = Nil
tip kx bm = Tip kx bm

empty :: IntSet
empty = Nil

singleton :: Key -> IntSet
singleton k = Tip (prefixOf k) (bitOf k)

null :: IntSet -> Bool
null Nil = True
null _ = False

size :: IntSet -> Int
size = go 0
  where
    go n Nil = n
    go n (Tip _ bm) = n + count bm
    go n (Bin _ _ l r) = go (go n l) r

member :: Key -> IntSet -> Bool
member k = go
  where
    go (Bin _ m l r) = if zero k m then go l else go r
    go (Tip kx bm) = prefixOf k == kx && bitOf k .&. bm /= 0
    go Nil = False

notMember :: Key -> IntSet -> Bool
notMember k t = not (member k t)

insert :: Key -> IntSet -> IntSet
insert k = insertBits (prefixOf k) (bitOf k)

insertBits :: Prefix -> BitMap -> IntSet -> IntSet
insertBits kx bm t = case t of
  Bin p m l r
    | nomatch kx p m -> link kx (Tip kx bm) p t
    | zero kx m -> Bin p m (insertBits kx bm l) r
    | otherwise -> Bin p m l (insertBits kx bm r)
  Tip kx' bm'
    | kx' == kx -> Tip kx (bm .|. bm')
    | otherwise -> link kx (Tip kx bm) kx' t
  Nil -> Tip kx bm

delete :: Key -> IntSet -> IntSet
delete k = deleteBits (prefixOf k) (bitOf k)

deleteBits :: Prefix -> BitMap -> IntSet -> IntSet
deleteBits kx bm t = case t of
  Bin p m l r
    | nomatch kx p m -> t
    | zero kx m -> bin p m (deleteBits kx bm l) r
    | otherwise -> bin p m l (deleteBits kx bm r)
  Tip kx' bm'
    | kx' == kx -> tip kx (bm' .&. complement bm)
    | otherwise -> t
  Nil -> Nil

-- The bits of the leaf an element would be in, if there is one.
bitsAt :: Prefix -> IntSet -> BitMap
bitsAt kx = go
  where
    go (Bin p m l r)
      | nomatch kx p m = 0
      | zero kx m = go l
      | otherwise = go r
    go (Tip kx' bm) = if kx' == kx then bm else 0
    go Nil = 0

union :: IntSet -> IntSet -> IntSet
union t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | m1 > m2 = if nomatch p2 p1 m1 then link p1 t1 p2 t2 else if zero p2 m1 then Bin p1 m1 (union l1 t2) r1 else Bin p1 m1 l1 (union r1 t2)
  | m2 > m1 = if nomatch p1 p2 m2 then link p1 t1 p2 t2 else if zero p1 m2 then Bin p2 m2 (union t1 l2) r2 else Bin p2 m2 l2 (union t1 r2)
  | p1 == p2 = Bin p1 m1 (union l1 l2) (union r1 r2)
  | otherwise = link p1 t1 p2 t2
union (Tip kx bm) t2 = insertBits kx bm t2
union t1 (Tip kx bm) = insertBits kx bm t1
union Nil t2 = t2
union t1 Nil = t1

unions :: [IntSet] -> IntSet
unions = List.foldl' union empty

difference :: IntSet -> IntSet -> IntSet
difference t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | m1 > m2 = if nomatch p2 p1 m1 then t1 else if zero p2 m1 then bin p1 m1 (difference l1 t2) r1 else bin p1 m1 l1 (difference r1 t2)
  | m2 > m1 = if nomatch p1 p2 m2 then t1 else if zero p1 m2 then difference t1 l2 else difference t1 r2
  | p1 == p2 = bin p1 m1 (difference l1 l2) (difference r1 r2)
  | otherwise = t1
difference (Tip kx bm) t2 = tip kx (bm .&. complement (bitsAt kx t2))
difference t1 (Tip kx bm) = deleteBits kx bm t1
difference Nil _ = Nil
difference t1 Nil = t1

(\\) :: IntSet -> IntSet -> IntSet
s1 \\ s2 = difference s1 s2

intersection :: IntSet -> IntSet -> IntSet
intersection t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | m1 > m2 = if nomatch p2 p1 m1 then Nil else if zero p2 m1 then intersection l1 t2 else intersection r1 t2
  | m2 > m1 = if nomatch p1 p2 m2 then Nil else if zero p1 m2 then intersection t1 l2 else intersection t1 r2
  | p1 == p2 = bin p1 m1 (intersection l1 l2) (intersection r1 r2)
  | otherwise = Nil
intersection (Tip kx bm) t2 = tip kx (bm .&. bitsAt kx t2)
intersection t1 (Tip kx bm) = tip kx (bm .&. bitsAt kx t1)
intersection Nil _ = Nil
intersection _ Nil = Nil

isSubsetOf :: IntSet -> IntSet -> Bool
isSubsetOf t1 t2 = null (difference t1 t2)

isProperSubsetOf :: IntSet -> IntSet -> Bool
isProperSubsetOf t1 t2 = isSubsetOf t1 t2 && size t1 < size t2

disjoint :: IntSet -> IntSet -> Bool
disjoint t1 t2 = null (intersection t1 t2)

-- Over the elements, least first. A branch on the sign has the ones
-- below nothing on its right; a leaf's are gone through a bit at a time.

foldr :: (Key -> b -> b) -> b -> IntSet -> b
foldr f z t = case t of
  Bin _ 63 l r -> go (go z l) r
  _ -> go z t
  where
    go z' Nil = z'
    go z' (Tip kx bm) = up kx bm z'
    go z' (Bin _ _ l r) = go (go z' r) l
    up kx bm z'
      | bm == 0 = z'
      | otherwise = f (kx + lowest bm) (up kx (bm .&. (bm - 1)) z')

foldl :: (b -> Key -> b) -> b -> IntSet -> b
foldl f z t = case t of
  Bin _ 63 l r -> go (go z r) l
  _ -> go z t
  where
    go z' Nil = z'
    go z' (Tip kx bm) = down kx bm z'
    go z' (Bin _ _ l r) = go (go z' l) r
    down kx bm z'
      | bm == 0 = z'
      | otherwise = let i = highest bm in f (down kx (bm `xor` (1 `shiftL` i)) z') (kx + i)

foldr' :: (Key -> b -> b) -> b -> IntSet -> b
foldr' f = foldr (\k z -> z `seq` f k z)

foldl' :: (b -> Key -> b) -> b -> IntSet -> b
foldl' f = foldl (\z k -> z `seq` f z k)

elems :: IntSet -> [Key]
elems = toAscList

toList :: IntSet -> [Key]
toList = toAscList

toAscList :: IntSet -> [Key]
toAscList = foldr (:) []

toDescList :: IntSet -> [Key]
toDescList = foldl (flip (:)) []

fromList :: [Key] -> IntSet
fromList = List.foldl' (flip insert) Nil

fromAscList :: [Key] -> IntSet
fromAscList = fromList

fromDistinctAscList :: [Key] -> IntSet
fromDistinctAscList = fromList

filter :: (Key -> Bool) -> IntSet -> IntSet
filter p = go
  where
    go (Bin q m l r) = bin q m (go l) (go r)
    go (Tip kx bm) = tip kx (kept kx bm)
    go Nil = Nil
    kept kx bm
      | bm == 0 = 0
      | otherwise = let i = lowest bm; rest = kept kx (bm .&. (bm - 1)) in if p (kx + i) then rest .|. (1 `shiftL` i) else rest

partition :: (Key -> Bool) -> IntSet -> (IntSet, IntSet)
partition p t = (filter p t, filter (not . p) t)

split :: Key -> IntSet -> (IntSet, IntSet)
split k t = (filter (< k) t, filter (> k) t)

splitMember :: Key -> IntSet -> (IntSet, Bool, IntSet)
splitMember k t = case split k t of
  (below, above) -> (below, member k t, above)

map :: (Key -> Key) -> IntSet -> IntSet
map f = fromList . List.map f . toAscList

-- The least and the greatest of a tree no part of which is on the sign.
least, greatest :: IntSet -> Maybe Key
least Nil = Nothing
least (Tip kx bm) = Just (kx + lowest bm)
least (Bin _ _ l _) = least l
greatest Nil = Nothing
greatest (Tip kx bm) = Just (kx + highest bm)
greatest (Bin _ _ _ r) = greatest r

lookupMin :: IntSet -> Maybe Key
lookupMin t = case t of
  Bin _ 63 _ r -> least r
  _ -> least t

lookupMax :: IntSet -> Maybe Key
lookupMax t = case t of
  Bin _ 63 l _ -> greatest l
  _ -> greatest t

findMin :: IntSet -> Key
findMin t = maybe (error "findMin: empty set has no minimal element") id (lookupMin t)

findMax :: IntSet -> Key
findMax t = maybe (error "findMax: empty set has no maximal element") id (lookupMax t)

minView :: IntSet -> Maybe (Key, IntSet)
minView t = fmap (\k -> (k, delete k t)) (lookupMin t)

maxView :: IntSet -> Maybe (Key, IntSet)
maxView t = fmap (\k -> (k, delete k t)) (lookupMax t)

deleteMin :: IntSet -> IntSet
deleteMin t = maybe Nil snd (minView t)

deleteMax :: IntSet -> IntSet
deleteMax t = maybe Nil snd (maxView t)

deleteFindMin :: IntSet -> (Key, IntSet)
deleteFindMin t = maybe (error "deleteFindMin: empty set has no minimal element", Nil) id (minView t)

deleteFindMax :: IntSet -> (Key, IntSet)
deleteFindMax t = maybe (error "deleteFindMax: empty set has no maximal element", Nil) id (maxView t)

lookupLT :: Key -> IntSet -> Maybe Key
lookupLT k t = case t of
  Bin _ 63 l r -> if k >= 0 then go r l else go Nil r
  _ -> go Nil t
  where
    kx = prefixOf k
    go below (Bin p m l r)
      | nomatch k p m = if k < p then greatest below else greatest r
      | zero k m = go below l
      | otherwise = go l r
    go below (Tip kx' bm)
      | kx' > kx = greatest below
      | kx' < kx = Just (kx' + highest bm)
      | under == 0 = greatest below
      | otherwise = Just (kx' + highest under)
      where
        under = bm .&. (bitOf k - 1)
    go below Nil = greatest below

lookupGT :: Key -> IntSet -> Maybe Key
lookupGT k t = case t of
  Bin _ 63 l r -> if k >= 0 then go Nil l else go l r
  _ -> go Nil t
  where
    kx = prefixOf k
    go above (Bin p m l r)
      | nomatch k p m = if k < p then least l else least above
      | zero k m = go r l
      | otherwise = go above r
    go above (Tip kx' bm)
      | kx' < kx = least above
      | kx' > kx = Just (kx' + lowest bm)
      | over == 0 = least above
      | otherwise = Just (kx' + lowest over)
      where
        -- The bits above the element's own.
        over = bm .&. complement (bitOf k .|. (bitOf k - 1))
    go above Nil = least above

lookupLE :: Key -> IntSet -> Maybe Key
lookupLE k t = if member k t then Just k else lookupLT k t

lookupGE :: Key -> IntSet -> Maybe Key
lookupGE k t = if member k t then Just k else lookupGT k t

instance Eq IntSet where
  t1 == t2 = toAscList t1 == toAscList t2

instance Ord IntSet where
  compare t1 t2 = compare (toAscList t1) (toAscList t2)

instance Show IntSet where
  showsPrec d t = showParen (d > 10) (showString "fromList " . shows (toAscList t))

instance Semigroup IntSet where
  (<>) = union

instance Monoid IntSet where
  mempty = empty
