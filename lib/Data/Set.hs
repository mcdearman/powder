{-# LANGUAGE GHC2021 #-}
-- Sets of things that have an order: trees kept balanced by their sizes,
-- as the containers package has them.
module Data.Set
  ( Set,
    empty,
    singleton,
    fromList,
    fromAscList,
    fromDistinctAscList,
    insert,
    delete,
    member,
    notMember,
    lookupLT,
    lookupGT,
    lookupLE,
    lookupGE,
    null,
    size,
    isSubsetOf,
    isProperSubsetOf,
    disjoint,
    union,
    unions,
    difference,
    (\\),
    intersection,
    filter,
    partition,
    split,
    splitMember,
    lookupIndex,
    findIndex,
    elemAt,
    deleteAt,
    take,
    drop,
    splitAt,
    map,
    mapMonotonic,
    foldr,
    foldl,
    foldr',
    foldl',
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
    elems,
    toList,
    toAscList,
    toDescList,
  )
where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import Prelude hiding (drop, filter, foldl, foldr, map, null, splitAt, take)

-- A tree is nothing, or a thing between those below it and those above,
-- with how many it has in all.
data Set a = Tip | Bin Int a (Set a) (Set a)

infixl 9 \\

empty :: Set a
empty = Tip

singleton :: a -> Set a
singleton x = Bin 1 x Tip Tip

null :: Set a -> Bool
null Tip = True
null _ = False

size :: Set a -> Int
size Tip = 0
size (Bin n _ _ _) = n

member :: Ord a => a -> Set a -> Bool
member _ Tip = False
member x (Bin _ y l r) = case compare x y of
  LT -> member x l
  GT -> member x r
  EQ -> True

notMember :: Ord a => a -> Set a -> Bool
notMember x s = not (member x s)

lookupLT :: Ord a => a -> Set a -> Maybe a
lookupLT x = go Nothing
  where
    go best Tip = best
    go best (Bin _ y l r)
      | y < x = go (Just y) r
      | otherwise = go best l

lookupGT :: Ord a => a -> Set a -> Maybe a
lookupGT x = go Nothing
  where
    go best Tip = best
    go best (Bin _ y l r)
      | y > x = go (Just y) l
      | otherwise = go best r

lookupLE :: Ord a => a -> Set a -> Maybe a
lookupLE x = go Nothing
  where
    go best Tip = best
    go best (Bin _ y l r) = case compare y x of
      LT -> go (Just y) r
      EQ -> Just y
      GT -> go best l

lookupGE :: Ord a => a -> Set a -> Maybe a
lookupGE x = go Nothing
  where
    go best Tip = best
    go best (Bin _ y l r) = case compare y x of
      GT -> go (Just y) l
      EQ -> Just y
      LT -> go best r

-- Keeping a tree balanced: neither side more than three times the other.

bin :: a -> Set a -> Set a -> Set a
bin x l r = Bin (size l + size r + 1) x l r

balance :: a -> Set a -> Set a -> Set a
balance x l r
  | sl + sr <= 1 = Bin (sl + sr + 1) x l r
  | sr > 3 * sl = rotateL x l r
  | sl > 3 * sr = rotateR x l r
  | otherwise = Bin (sl + sr + 1) x l r
  where
    sl = size l
    sr = size r

rotateL :: a -> Set a -> Set a -> Set a
rotateL x l r@(Bin _ _ ly ry)
  | size ly < 2 * size ry = singleL x l r
  | otherwise = doubleL x l r
rotateL x l Tip = bin x l Tip

rotateR :: a -> Set a -> Set a -> Set a
rotateR x l@(Bin _ _ ly ry) r
  | size ry < 2 * size ly = singleR x l r
  | otherwise = doubleR x l r
rotateR x Tip r = bin x Tip r

singleL, singleR, doubleL, doubleR :: a -> Set a -> Set a -> Set a
singleL x1 t1 (Bin _ x2 t2 t3) = bin x2 (bin x1 t1 t2) t3
singleL x1 t1 Tip = bin x1 t1 Tip
singleR x1 (Bin _ x2 t1 t2) t3 = bin x2 t1 (bin x1 t2 t3)
singleR x1 Tip t3 = bin x1 Tip t3
doubleL x1 t1 (Bin _ x2 (Bin _ x3 t2 t3) t4) = bin x3 (bin x1 t1 t2) (bin x2 t3 t4)
doubleL x1 t1 t2 = singleL x1 t1 t2
doubleR x1 (Bin _ x2 t1 (Bin _ x3 t2 t3)) t4 = bin x3 (bin x2 t1 t2) (bin x1 t3 t4)
doubleR x1 t1 t2 = singleR x1 t1 t2

insert :: Ord a => a -> Set a -> Set a
insert x Tip = singleton x
insert x (Bin n y l r) = case compare x y of
  LT -> balance y (insert x l) r
  GT -> balance y l (insert x r)
  EQ -> Bin n x l r

delete :: Ord a => a -> Set a -> Set a
delete _ Tip = Tip
delete x (Bin _ y l r) = case compare x y of
  LT -> balance y (delete x l) r
  GT -> balance y l (delete x r)
  EQ -> glue l r

lookupMin :: Set a -> Maybe a
lookupMin Tip = Nothing
lookupMin (Bin _ x Tip _) = Just x
lookupMin (Bin _ _ l _) = lookupMin l

lookupMax :: Set a -> Maybe a
lookupMax Tip = Nothing
lookupMax (Bin _ x _ Tip) = Just x
lookupMax (Bin _ _ _ r) = lookupMax r

findMin :: Set a -> a
findMin s = maybe (error "Set.findMin: empty set has no minimal element") id (lookupMin s)

findMax :: Set a -> a
findMax s = maybe (error "Set.findMax: empty set has no maximal element") id (lookupMax s)

deleteMin :: Set a -> Set a
deleteMin Tip = Tip
deleteMin (Bin _ _ Tip r) = r
deleteMin (Bin _ x l r) = balance x (deleteMin l) r

deleteMax :: Set a -> Set a
deleteMax Tip = Tip
deleteMax (Bin _ _ l Tip) = l
deleteMax (Bin _ x l r) = balance x l (deleteMax r)

minView :: Set a -> Maybe (a, Set a)
minView Tip = Nothing
minView (Bin _ x Tip r) = Just (x, r)
minView (Bin _ x l r) = case minView l of
  Just (least, l') -> Just (least, balance x l' r)
  Nothing -> Nothing

maxView :: Set a -> Maybe (a, Set a)
maxView Tip = Nothing
maxView (Bin _ x l Tip) = Just (x, l)
maxView (Bin _ x l r) = case maxView r of
  Just (greatest, r') -> Just (greatest, balance x l r')
  Nothing -> Nothing

deleteFindMin :: Set a -> (a, Set a)
deleteFindMin s = case minView s of
  Just found -> found
  Nothing -> (error "Set.deleteFindMin: can not return the minimal element of an empty set", Tip)

deleteFindMax :: Set a -> (a, Set a)
deleteFindMax s = case maxView s of
  Just found -> found
  Nothing -> (error "Set.deleteFindMax: can not return the maximal element of an empty set", Tip)

glue :: Set a -> Set a -> Set a
glue Tip r = r
glue l Tip = l
glue l r
  | size l > size r = case maxView l of
      Just (x, l') -> balance x l' r
      Nothing -> r
  | otherwise = case minView r of
      Just (x, r') -> balance x l r'
      Nothing -> l

merge :: Set a -> Set a -> Set a
merge Tip r = r
merge l Tip = l
merge l@(Bin sl x lx rx) r@(Bin sr y ly ry)
  | 3 * sl < sr = balance y (merge l ly) ry
  | 3 * sr < sl = balance x lx (merge rx r)
  | otherwise = glue l r

link :: a -> Set a -> Set a -> Set a
link x Tip r = insertMin x r
link x l Tip = insertMax x l
link x l@(Bin sl y ly ry) r@(Bin sr z lz rz)
  | 3 * sl < sr = balance z (link x l lz) rz
  | 3 * sr < sl = balance y ly (link x ry r)
  | otherwise = bin x l r

insertMin, insertMax :: a -> Set a -> Set a
insertMin x Tip = singleton x
insertMin x (Bin _ y l r) = balance y (insertMin x l) r
insertMax x Tip = singleton x
insertMax x (Bin _ y l r) = balance y l (insertMax x r)

split :: Ord a => a -> Set a -> (Set a, Set a)
split x s = case splitMember x s of
  (l, _, r) -> (l, r)

splitMember :: Ord a => a -> Set a -> (Set a, Bool, Set a)
splitMember _ Tip = (Tip, False, Tip)
splitMember x (Bin _ y l r) = case compare x y of
  LT -> case splitMember x l of
    (below, found, above) -> (below, found, link y above r)
  GT -> case splitMember x r of
    (below, found, above) -> (link y l below, found, above)
  EQ -> (l, True, r)

union :: Ord a => Set a -> Set a -> Set a
union t1 Tip = t1
union Tip t2 = t2
union (Bin _ x l r) t2 = case split x t2 of
  (l2, r2) -> link x (union l l2) (union r r2)

unions :: Ord a => [Set a] -> Set a
unions = List.foldl' union empty

difference :: Ord a => Set a -> Set a -> Set a
difference Tip _ = Tip
difference t1 Tip = t1
difference t1 (Bin _ x l2 r2) = case split x t1 of
  (l1, r1) -> merge (difference l1 l2) (difference r1 r2)

(\\) :: Ord a => Set a -> Set a -> Set a
s1 \\ s2 = difference s1 s2

intersection :: Ord a => Set a -> Set a -> Set a
intersection Tip _ = Tip
intersection _ Tip = Tip
intersection (Bin _ x l r) t2 = case splitMember x t2 of
  (l2, found, r2) ->
    let l' = intersection l l2
        r' = intersection r r2
     in if found then link x l' r' else merge l' r'

isSubsetOf :: Ord a => Set a -> Set a -> Bool
isSubsetOf s1 s2 = size s1 <= size s2 && all (`member` s2) (toAscList s1)

isProperSubsetOf :: Ord a => Set a -> Set a -> Bool
isProperSubsetOf s1 s2 = size s1 < size s2 && isSubsetOf s1 s2

disjoint :: Ord a => Set a -> Set a -> Bool
disjoint s1 s2 = null (intersection s1 s2)

filter :: (a -> Bool) -> Set a -> Set a
filter _ Tip = Tip
filter p (Bin _ x l r)
  | p x = link x (filter p l) (filter p r)
  | otherwise = merge (filter p l) (filter p r)

partition :: (a -> Bool) -> Set a -> (Set a, Set a)
partition p s = (filter p s, filter (not . p) s)

map :: Ord b => (a -> b) -> Set a -> Set b
map f s = fromList (List.map f (toAscList s))

mapMonotonic :: (a -> b) -> Set a -> Set b
mapMonotonic _ Tip = Tip
mapMonotonic f (Bin n x l r) = Bin n (f x) (mapMonotonic f l) (mapMonotonic f r)

foldr :: (a -> b -> b) -> b -> Set a -> b
foldr _ z Tip = z
foldr f z (Bin _ x l r) = foldr f (f x (foldr f z r)) l

foldl :: (b -> a -> b) -> b -> Set a -> b
foldl _ z Tip = z
foldl f z (Bin _ x l r) = foldl f (f (foldl f z l) x) r

foldr' :: (a -> b -> b) -> b -> Set a -> b
foldr' _ z Tip = z
foldr' f z (Bin _ x l r) = let z' = foldr' f z r in z' `seq` (let z'' = f x z' in z'' `seq` foldr' f z'' l)

foldl' :: (b -> a -> b) -> b -> Set a -> b
foldl' _ z Tip = z
foldl' f z (Bin _ x l r) = let z' = foldl' f z l in z' `seq` (let z'' = f z' x in z'' `seq` foldl' f z'' r)

elems :: Set a -> [a]
elems = toAscList

toList :: Set a -> [a]
toList = toAscList

toAscList :: Set a -> [a]
toAscList = foldr (:) []

toDescList :: Set a -> [a]
toDescList = foldl (flip (:)) []

fromList :: Ord a => [a] -> Set a
fromList = List.foldl' (flip insert) empty

fromAscList :: Eq a => [a] -> Set a
fromAscList = fromDistinctAscList . once
  where
    once (x : rest@(y : _)) | x == y = once rest
    once (x : rest) = x : once rest
    once [] = []

-- From things already in order, each once: the middle one at the top.
fromDistinctAscList :: [a] -> Set a
fromDistinctAscList xs = fst (build (length xs) xs)
  where
    build 0 rest = (Tip, rest)
    build n rest =
      let half = n `div` 2
          (l, after) = build half rest
       in case after of
            x : more -> let (r, end) = build (n - half - 1) more in (bin x l r, end)
            [] -> (l, [])

lookupIndex :: Ord a => a -> Set a -> Maybe Int
lookupIndex = go 0
  where
    go _ _ Tip = Nothing
    go before x (Bin _ y l r) = case compare x y of
      LT -> go before x l
      GT -> go (before + size l + 1) x r
      EQ -> Just (before + size l)

findIndex :: Ord a => a -> Set a -> Int
findIndex x s = maybe (error "Set.findIndex: element is not in the set") id (lookupIndex x s)

elemAt :: Int -> Set a -> a
elemAt _ Tip = error "Set.elemAt: index out of range"
elemAt i (Bin _ x l r) = case compare i (size l) of
  LT -> elemAt i l
  GT -> elemAt (i - size l - 1) r
  EQ -> x

deleteAt :: Int -> Set a -> Set a
deleteAt _ Tip = error "Set.deleteAt: index out of range"
deleteAt i (Bin _ x l r) = case compare i (size l) of
  LT -> balance x (deleteAt i l) r
  GT -> balance x l (deleteAt (i - size l - 1) r)
  EQ -> glue l r

take :: Int -> Set a -> Set a
take n s = fst (splitAt n s)

drop :: Int -> Set a -> Set a
drop n s = snd (splitAt n s)

splitAt :: Int -> Set a -> (Set a, Set a)
splitAt _ Tip = (Tip, Tip)
splitAt n s@(Bin _ x l r)
  | n <= 0 = (Tip, s)
  | n >= size s = (s, Tip)
  | otherwise = case compare n (size l) of
      LT -> case splitAt n l of
        (first, rest) -> (first, link x rest r)
      GT -> case splitAt (n - size l - 1) r of
        (first, rest) -> (link x l first, rest)
      EQ -> (l, insertMin x r)

instance Eq a => Eq (Set a) where
  s1 == s2 = size s1 == size s2 && toAscList s1 == toAscList s2

instance Ord a => Ord (Set a) where
  compare s1 s2 = compare (toAscList s1) (toAscList s2)

instance Show a => Show (Set a) where
  showsPrec d s = showParen (d > 10) (showString "fromList " . shows (toAscList s))

instance Foldable.Foldable Set where
  foldr = foldr
  foldMap f = foldr (\x rest -> f x <> rest) mempty
  length = size
  null = null
  toList = toAscList
  elem x s = any (== x) (toAscList s)

instance Ord a => Semigroup (Set a) where
  (<>) = union

instance Ord a => Monoid (Set a) where
  mempty = empty
