-- Arrays: a balanced tree under the indices, so that looking one element
-- up, or replacing one, takes a number of steps that grows with the
-- logarithm of the size.
module Data.Array
  ( module Data.Ix, Array, array, listArray, (!), bounds, indices, elems, assocs, accumArray, (//), accum, ixmap
  )
where

import Data.Ix

infixl 9 !, //

-- The elements in order: those before the middle, the middle, the rest.
data Tree e = Leaf | Node (Tree e) e (Tree e)

-- Bounds, how many elements, and the elements.
data Array i e = MkArray (i, i) Int (Tree e)

blank :: Int -> Tree e
blank 0 = Leaf
blank n = let h = n `div` 2 in Node (blank h) (error "(Array.!): undefined array element") (blank (n - h - 1))

at :: Int -> Tree e -> Int -> e
at n (Node l x r) i =
  let h = n `div` 2
   in if i < h then at h l i else if i == h then x else at (n - h - 1) r (i - h - 1)
at _ Leaf _ = error "Array: index out of range"

change :: (e -> e) -> Int -> Int -> Tree e -> Tree e
change f n i (Node l x r) =
  let h = n `div` 2
   in if i < h then Node (change f h i l) x r else if i == h then Node l (f x) r else Node l x (change f (n - h - 1) (i - h - 1) r)
change _ _ _ Leaf = error "Array: index out of range"

flatten :: Tree e -> [e] -> [e]
flatten Leaf rest = rest
flatten (Node l x r) rest = flatten l (x : flatten r rest)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Leaf = Leaf
mapTree f (Node l x r) = Node (mapTree f l) (f x) (mapTree f r)

array :: Ix i => (i, i) -> [(i, e)] -> Array i e
array b ivs = MkArray b n (foldl (\t (i, v) -> change (const v) n (index b i) t) (blank n) ivs)
  where
    n = rangeSize b

listArray :: Ix i => (i, i) -> [e] -> Array i e
listArray b vs = array b (zip (range b) vs)

(!) :: Ix i => Array i e -> i -> e
MkArray b n t ! i = at n t (index b i)

bounds :: Array i e -> (i, i)
bounds (MkArray b _ _) = b

indices :: Ix i => Array i e -> [i]
indices a = range (bounds a)

elems :: Array i e -> [e]
elems (MkArray _ _ t) = flatten t []

assocs :: Ix i => Array i e -> [(i, e)]
assocs a = zip (indices a) (elems a)

(//) :: Ix i => Array i e -> [(i, e)] -> Array i e
MkArray b n t // ivs = MkArray b n (foldl (\u (i, v) -> change (const v) n (index b i) u) t ivs)

accum :: Ix i => (e -> a -> e) -> Array i e -> [(i, a)] -> Array i e
accum f (MkArray b n t) ivs = MkArray b n (foldl (\u (i, v) -> change (\old -> f old v) n (index b i) u) t ivs)

accumArray :: Ix i => (e -> a -> e) -> e -> (i, i) -> [(i, a)] -> Array i e
accumArray f z b = accum f (array b [(i, z) | i <- range b])

ixmap :: (Ix i, Ix j) => (i, i) -> (i -> j) -> Array j e -> Array i e
ixmap b f a = array b [(i, a ! f i) | i <- range b]

instance Functor (Array i) where
  fmap f (MkArray b n t) = MkArray b n (mapTree f t)

instance (Ix i, Eq e) => Eq (Array i e) where
  a == b = assocs a == assocs b

instance (Ix i, Ord e) => Ord (Array i e) where
  compare a b = compare (assocs a) (assocs b)

instance (Ix i, Read i, Read e) => Read (Array i e) where
  readsPrec p = readParen (p > 10) (\r -> [(array b as, u) | ("array", s) <- lex r, (b, t) <- readsPrec 11 s, (as, u) <- readsPrec 11 t])

instance (Ix i, Show i, Show e) => Show (Array i e) where
  showsPrec p a = showParen (p > 10) (showString "array " . showsPrec 11 (bounds a) . showChar ' ' . showsPrec 11 (assocs a))
