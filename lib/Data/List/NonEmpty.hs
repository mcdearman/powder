-- Lists that have at least one element. Most of what is here has the name
-- of a function of lists, so the module is one to import qualified.
module Data.List.NonEmpty
  ( NonEmpty (..), map, head, tail, last, init, length, toList, fromList, nonEmpty, cons, (<|), appendList,
    reverse, sort, sortBy, zip, zipWith, singleton, filter, takeWhile, dropWhile, take, drop, group, groupBy,
    nub, iterate, repeat, cycle, xor
  )
where

import qualified Data.List as L
import Powder.Base (Eq (..), Maybe (..), NonEmpty (..), Ord (..), Ordering, error, foldr, not, (.))
import Prelude ()
import qualified Powder.Base as B

infixr 5 <|

map :: (a -> b) -> NonEmpty a -> NonEmpty b
map f (x :| xs) = f x :| L.map f xs

head :: NonEmpty a -> a
head (x :| _) = x

tail :: NonEmpty a -> [a]
tail (_ :| xs) = xs

last :: NonEmpty a -> a
last (x :| xs) = L.last (x : xs)

init :: NonEmpty a -> [a]
init (x :| xs) = L.init (x : xs)

length :: NonEmpty a -> Int
length (_ :| xs) = 1 B.+ L.length xs

toList :: NonEmpty a -> [a]
toList (x :| xs) = x : xs

fromList :: [a] -> NonEmpty a
fromList (x : xs) = x :| xs
fromList [] = error "NonEmpty.fromList: empty list"

nonEmpty :: [a] -> Maybe (NonEmpty a)
nonEmpty [] = Nothing
nonEmpty (x : xs) = Just (x :| xs)

cons :: a -> NonEmpty a -> NonEmpty a
cons a (x :| xs) = a :| (x : xs)

(<|) :: a -> NonEmpty a -> NonEmpty a
(<|) = cons

appendList :: NonEmpty a -> [a] -> NonEmpty a
appendList (x :| xs) ys = x :| (xs L.++ ys)

lift :: ([a] -> [b]) -> NonEmpty a -> NonEmpty b
lift f = fromList . f . toList

reverse :: NonEmpty a -> NonEmpty a
reverse = lift L.reverse

sort :: Ord a => NonEmpty a -> NonEmpty a
sort = lift L.sort

sortBy :: (a -> a -> Ordering) -> NonEmpty a -> NonEmpty a
sortBy f = lift (L.sortBy f)

zip :: NonEmpty a -> NonEmpty b -> NonEmpty (a, b)
zip (x :| xs) (y :| ys) = (x, y) :| L.zip xs ys

zipWith :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
zipWith f (x :| xs) (y :| ys) = f x y :| L.zipWith f xs ys

singleton :: a -> NonEmpty a
singleton a = a :| []

filter :: (a -> Bool) -> NonEmpty a -> [a]
filter p = L.filter p . toList

takeWhile :: (a -> Bool) -> NonEmpty a -> [a]
takeWhile p = L.takeWhile p . toList

dropWhile :: (a -> Bool) -> NonEmpty a -> [a]
dropWhile p = L.dropWhile p . toList

take :: Int -> NonEmpty a -> [a]
take n = L.take n . toList

drop :: Int -> NonEmpty a -> [a]
drop n = L.drop n . toList

group :: Eq a => [a] -> [NonEmpty a]
group = groupBy (==)

groupBy :: (a -> a -> Bool) -> [a] -> [NonEmpty a]
groupBy eq = L.map fromList . L.groupBy eq

nub :: Eq a => NonEmpty a -> NonEmpty a
nub = lift L.nub

iterate :: (a -> a) -> a -> NonEmpty a
iterate f a = a :| L.iterate f (f a)

repeat :: a -> NonEmpty a
repeat a = a :| L.repeat a

cycle :: NonEmpty a -> NonEmpty a
cycle = fromList . L.cycle . toList

xor :: NonEmpty Bool -> Bool
xor (x :| xs) = foldr xor' x xs
  where
    xor' True y = not y
    xor' False y = y
