{-# LANGUAGE GHC2021 #-}
-- Text, as the text package has it: here a list of characters under a
-- type of its own, so that a program written for that package means the
-- same, and takes the time a list does. Most of what is here has the
-- name of a function of lists, so the module is one to import qualified.
module Data.Text
  ( Text, pack, unpack, singleton, empty, cons, snoc, append, uncons, unsnoc, head, last, tail, init, null,
    length, compareLength, map, intercalate, intersperse, transpose, reverse, replace, toCaseFold, toLower,
    toUpper, toTitle, justifyLeft, justifyRight, center, foldl, foldl', foldr, concat, concatMap, any, all,
    maximum, minimum, take, takeEnd, drop, dropEnd, takeWhile, takeWhileEnd, dropWhile, dropWhileEnd, dropAround,
    strip, stripStart, stripEnd, splitAt, breakOn, breakOnEnd, break, span, group, groupBy, inits, tails,
    splitOn, split, chunksOf, lines, words, unlines, unwords, isPrefixOf, isSuffixOf, isInfixOf, stripPrefix,
    stripSuffix, commonPrefixes, filter, breakOnAll, find, elem, partition, index, findIndex, count, zip, zipWith,
    replicate, show
  )
where

import qualified Data.Char as C
import qualified Data.List as L
import Data.String (IsString (..))
import Prelude hiding
  ( all, any, break, concat, concatMap, drop, dropWhile, elem, filter, foldl, foldr, head, init, last, length,
    lines, map, maximum, minimum, null, replicate, reverse, show, span, splitAt, tail, take, takeWhile, unlines,
    unwords, words, zip, zipWith
  )
import qualified Prelude as P

newtype Text = Text String

instance Eq Text where
  Text a == Text b = a == b

instance Ord Text where
  compare (Text a) (Text b) = compare a b

instance Show Text where
  showsPrec d (Text s) = showsPrec d s

instance Read Text where
  readsPrec d s = [(Text x, rest) | (x, rest) <- readsPrec d s]

instance Semigroup Text where
  Text a <> Text b = Text (a ++ b)

instance Monoid Text where
  mempty = empty

instance IsString Text where
  fromString = pack

pack :: String -> Text
pack = Text

unpack :: Text -> String
unpack (Text s) = s

-- A value as it is shown, as text.
show :: Show a => a -> Text
show = Text . P.show

singleton :: Char -> Text
singleton c = Text [c]

empty :: Text
empty = Text ""

cons :: Char -> Text -> Text
cons c (Text s) = Text (c : s)

snoc :: Text -> Char -> Text
snoc (Text s) c = Text (s ++ [c])

append :: Text -> Text -> Text
append = (<>)

uncons :: Text -> Maybe (Char, Text)
uncons (Text []) = Nothing
uncons (Text (c : s)) = Just (c, Text s)

unsnoc :: Text -> Maybe (Text, Char)
unsnoc (Text []) = Nothing
unsnoc (Text s) = Just (Text (P.init s), P.last s)

head :: Text -> Char
head = P.head . unpack

last :: Text -> Char
last = P.last . unpack

tail :: Text -> Text
tail = Text . P.tail . unpack

init :: Text -> Text
init = Text . P.init . unpack

null :: Text -> Bool
null = P.null . unpack

length :: Text -> Int
length = P.length . unpack

compareLength :: Text -> Int -> Ordering
compareLength t n = compare (length t) n

map :: (Char -> Char) -> Text -> Text
map f = Text . P.map f . unpack

intercalate :: Text -> [Text] -> Text
intercalate (Text sep) ts = Text (L.intercalate sep (P.map unpack ts))

intersperse :: Char -> Text -> Text
intersperse c = Text . L.intersperse c . unpack

transpose :: [Text] -> [Text]
transpose = P.map Text . L.transpose . P.map unpack

reverse :: Text -> Text
reverse = Text . P.reverse . unpack

-- Every place one text is in another, with a third in its place.
replace :: Text -> Text -> Text -> Text
replace needle with haystack
  | null needle = error "Data.Text.replace: empty input"
  | otherwise = intercalate with (splitOn needle haystack)

-- As small letters, for comparing without regard to case. Of the letters
-- that fold to more than one, only the sharp s is known here.
toCaseFold :: Text -> Text
toCaseFold = Text . P.concatMap fold . unpack
  where
    fold c = if c == '\223' then "ss" else [C.toLower c]

toLower :: Text -> Text
toLower = map C.toLower

toUpper :: Text -> Text
toUpper = map C.toUpper

-- The first letter of each word a capital, and the rest small. A word is
-- what is between spaces.
toTitle :: Text -> Text
toTitle = Text . go True . unpack
  where
    go _ [] = []
    go start (c : rest)
      | C.isLetter c = (if start then C.toUpper c else C.toLower c) : go False rest
      | C.isSpace c = c : go True rest
      | otherwise = c : go start rest

justifyLeft :: Int -> Char -> Text -> Text
justifyLeft n c t = t <> replicateChar (n - length t) c

justifyRight :: Int -> Char -> Text -> Text
justifyRight n c t = replicateChar (n - length t) c <> t

center :: Int -> Char -> Text -> Text
center n c t
  | missing <= 0 = t
  | otherwise = replicateChar left c <> t <> replicateChar right c
  where
    missing = n - length t
    right = missing `div` 2
    left = missing - right

replicateChar :: Int -> Char -> Text
replicateChar n c = Text (P.replicate n c)

foldl :: (a -> Char -> a) -> a -> Text -> a
foldl f z = P.foldl f z . unpack

foldl' :: (a -> Char -> a) -> a -> Text -> a
foldl' f z = L.foldl' f z . unpack

foldr :: (Char -> a -> a) -> a -> Text -> a
foldr f z = P.foldr f z . unpack

concat :: [Text] -> Text
concat = Text . P.concatMap unpack

concatMap :: (Char -> Text) -> Text -> Text
concatMap f = Text . P.concatMap (unpack . f) . unpack

any :: (Char -> Bool) -> Text -> Bool
any p = P.any p . unpack

all :: (Char -> Bool) -> Text -> Bool
all p = P.all p . unpack

maximum :: Text -> Char
maximum = P.maximum . unpack

minimum :: Text -> Char
minimum = P.minimum . unpack

take :: Int -> Text -> Text
take n = Text . P.take n . unpack

takeEnd :: Int -> Text -> Text
takeEnd n t = drop (length t - n) t

drop :: Int -> Text -> Text
drop n = Text . P.drop n . unpack

dropEnd :: Int -> Text -> Text
dropEnd n t = take (length t - n) t

takeWhile :: (Char -> Bool) -> Text -> Text
takeWhile p = Text . P.takeWhile p . unpack

takeWhileEnd :: (Char -> Bool) -> Text -> Text
takeWhileEnd p = reverse . takeWhile p . reverse

dropWhile :: (Char -> Bool) -> Text -> Text
dropWhile p = Text . P.dropWhile p . unpack

dropWhileEnd :: (Char -> Bool) -> Text -> Text
dropWhileEnd p = Text . L.dropWhileEnd p . unpack

dropAround :: (Char -> Bool) -> Text -> Text
dropAround p = dropWhile p . dropWhileEnd p

strip :: Text -> Text
strip = dropAround C.isSpace

stripStart :: Text -> Text
stripStart = dropWhile C.isSpace

stripEnd :: Text -> Text
stripEnd = dropWhileEnd C.isSpace

splitAt :: Int -> Text -> (Text, Text)
splitAt n (Text s) = case P.splitAt n s of
  (a, b) -> (Text a, Text b)

-- Up to the first place one text is in another, and from there.
breakOn :: Text -> Text -> (Text, Text)
breakOn (Text needle) (Text haystack)
  | P.null needle = error "Data.Text.breakOn: empty input"
  | otherwise = go [] haystack
  where
    go before rest
      | needle `L.isPrefixOf` rest || P.null rest = (Text (P.reverse before), Text rest)
      | otherwise = go (P.head rest : before) (P.tail rest)

-- Through the last place one text is in another, and what is after.
breakOnEnd :: Text -> Text -> (Text, Text)
breakOnEnd needle haystack = case breakOn (reverse needle) (reverse haystack) of
  (after, through) -> (reverse through, reverse after)

-- Each place one text is in another: what is before it, and from there.
breakOnAll :: Text -> Text -> [(Text, Text)]
breakOnAll (Text needle) (Text haystack)
  | P.null needle = error "Data.Text.breakOnAll: empty input"
  | otherwise = go [] haystack
  where
    go _ [] = []
    go before rest@(c : more)
      | needle `L.isPrefixOf` rest = (Text (P.reverse before), Text rest) : skipped
      | otherwise = go (c : before) more
      where
        -- The next is looked for after the whole of this one.
        skipped = go (P.reverse (P.take (P.length needle) rest) ++ before) (P.drop (P.length needle) rest)

break :: (Char -> Bool) -> Text -> (Text, Text)
break p (Text s) = case P.break p s of
  (a, b) -> (Text a, Text b)

span :: (Char -> Bool) -> Text -> (Text, Text)
span p (Text s) = case P.span p s of
  (a, b) -> (Text a, Text b)

group :: Text -> [Text]
group = P.map Text . L.group . unpack

groupBy :: (Char -> Char -> Bool) -> Text -> [Text]
groupBy f = P.map Text . L.groupBy f . unpack

inits :: Text -> [Text]
inits = P.map Text . L.inits . unpack

tails :: Text -> [Text]
tails = P.map Text . L.tails . unpack

-- What is between the places one text is in another.
splitOn :: Text -> Text -> [Text]
splitOn (Text needle) (Text haystack)
  | P.null needle = error "Data.Text.splitOn: empty input"
  | otherwise = go [] haystack
  where
    n = P.length needle
    go piece rest
      | P.null rest = [Text (P.reverse piece)]
      | needle `L.isPrefixOf` rest = Text (P.reverse piece) : go [] (P.drop n rest)
      | otherwise = go (P.head rest : piece) (P.tail rest)

-- What is between the characters that are of some kind.
split :: (Char -> Bool) -> Text -> [Text]
split p (Text s) = go s
  where
    go rest = case P.break p rest of
      (piece, []) -> [Text piece]
      (piece, _ : more) -> Text piece : go more

chunksOf :: Int -> Text -> [Text]
chunksOf n (Text s)
  | n <= 0 = []
  | otherwise = go s
  where
    go [] = []
    go rest = Text (P.take n rest) : go (P.drop n rest)

lines :: Text -> [Text]
lines = P.map Text . P.lines . unpack

words :: Text -> [Text]
words = P.map Text . P.words . unpack

unlines :: [Text] -> Text
unlines = Text . P.unlines . P.map unpack

unwords :: [Text] -> Text
unwords = Text . P.unwords . P.map unpack

isPrefixOf :: Text -> Text -> Bool
isPrefixOf (Text a) (Text b) = a `L.isPrefixOf` b

isSuffixOf :: Text -> Text -> Bool
isSuffixOf (Text a) (Text b) = a `L.isSuffixOf` b

isInfixOf :: Text -> Text -> Bool
isInfixOf (Text a) (Text b) = a `L.isInfixOf` b

stripPrefix :: Text -> Text -> Maybe Text
stripPrefix (Text a) (Text b) = fmap Text (L.stripPrefix a b)

stripSuffix :: Text -> Text -> Maybe Text
stripSuffix (Text a) (Text b) = fmap (Text . P.reverse) (L.stripPrefix (P.reverse a) (P.reverse b))

-- What two texts start with alike, if anything, and the rest of each.
commonPrefixes :: Text -> Text -> Maybe (Text, Text, Text)
commonPrefixes (Text a) (Text b) = case go a b of
  ([], _, _) -> Nothing
  (common, restA, restB) -> Just (Text common, Text restA, Text restB)
  where
    go (x : xs) (y : ys) | x == y = let (common, restA, restB) = go xs ys in (x : common, restA, restB)
    go xs ys = ([], xs, ys)

filter :: (Char -> Bool) -> Text -> Text
filter p = Text . P.filter p . unpack

find :: (Char -> Bool) -> Text -> Maybe Char
find p = L.find p . unpack

elem :: Char -> Text -> Bool
elem c = P.elem c . unpack

partition :: (Char -> Bool) -> Text -> (Text, Text)
partition p (Text s) = case L.partition p s of
  (a, b) -> (Text a, Text b)

index :: Text -> Int -> Char
index (Text s) n = s !! n

findIndex :: (Char -> Bool) -> Text -> Maybe Int
findIndex p = L.findIndex p . unpack

-- How many times one text is in another, no two of them sharing any.
count :: Text -> Text -> Int
count needle haystack
  | null needle = error "Data.Text.count: empty input"
  | otherwise = P.length (splitOn needle haystack) - 1

zip :: Text -> Text -> [(Char, Char)]
zip (Text a) (Text b) = P.zip a b

zipWith :: (Char -> Char -> Char) -> Text -> Text -> Text
zipWith f (Text a) (Text b) = Text (P.zipWith f a b)

replicate :: Int -> Text -> Text
replicate n (Text s) = Text (P.concat (P.replicate n s))
