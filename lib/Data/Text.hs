{-# LANGUAGE GHC2021 #-}
-- Text, as the text package has it: the bytes of its characters in UTF-8,
-- side by side in memory, and a text a run of them, by where it starts
-- and how many bytes it is. So a part of a text is the same bytes and no
-- copy, two texts are compared a run of bytes at a time, and a text of
-- letters and digits is a byte for each. Most of what is here has the
-- name of a function of lists, so the module is one to import qualified.
module Data.Text
  ( Text, pack, unpack, singleton, empty, cons, snoc, append, uncons, unsnoc, head, last, tail, init, null,
    length, compareLength, map, intercalate, intersperse, transpose, reverse, replace, toCaseFold, toLower,
    toUpper, toTitle, justifyLeft, justifyRight, center, foldl, foldl', foldr, concat, concatMap, any, all,
    maximum, minimum, take, takeEnd, drop, dropEnd, takeWhile, takeWhileEnd, dropWhile, dropWhileEnd, dropAround,
    strip, stripStart, stripEnd, splitAt, breakOn, breakOnEnd, break, span, group, groupBy, inits, tails,
    splitOn, split, chunksOf, lines, words, unlines, unwords, isPrefixOf, isSuffixOf, isInfixOf, stripPrefix,
    stripSuffix, commonPrefixes, filter, breakOnAll, find, elem, partition, index, findIndex, count, zip, zipWith,
    replicate, show, copy
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

-- Bytes side by side, which the machines have: what the primitives call
-- a number is them.
newtype Bytes = Bytes Integer

-- The bytes, the place of the first that is this text's, and how many of
-- them are.
data Text = Text !Bytes !Int !Int

-- Of bytes.

byteAt :: Bytes -> Int -> Int
byteAt (Bytes a) i = fromInteger (primBytesRead a (toInteger i))

-- Whether two runs as long as each other are the same: below nothing,
-- nothing, or above, as the first is to the second.
ordered :: Bytes -> Int -> Bytes -> Int -> Int -> Int
ordered (Bytes a) i (Bytes b) j n = fromInteger (primBytesCompare a (toInteger i) b (toInteger j) (toInteger n))

-- New bytes, written by an action before anything reads them.
made :: Int -> (Integer -> IO ()) -> Text
made n fill
  | n <= 0 = empty
  | otherwise = primPerformIO (primBytesNew (toInteger n) >>= \a -> fill a >> return (Text (Bytes a) 0 n))

copied :: Integer -> Int -> Text -> IO ()
copied to at (Text (Bytes a) off len) = primBytesCopy to (toInteger at) a (toInteger off) (toInteger len)

-- Of characters as bytes.

-- How many bytes a character is, and one that cannot be written, which
-- half of a pair is, as the mark for that.
width :: Char -> Int
width c
  | n < 0x80 = 1
  | n < 0x800 = 2
  | n < 0x10000 = 3
  | otherwise = 4
  where
    n = C.ord c

sound :: Char -> Char
sound c = if n >= 0xD800 && n <= 0xDFFF then '\xFFFD' else c
  where
    n = C.ord c

-- How many bytes the character that starts with a byte is.
widthFrom :: Int -> Int
widthFrom b
  | b < 0x80 = 1
  | b < 0xE0 = 2
  | b < 0xF0 = 3
  | otherwise = 4

-- The character at a place, and how many bytes it is.
charAt :: Bytes -> Int -> (Char, Int)
charAt a i
  | b < 0x80 = (C.chr b, 1)
  | b < 0xE0 = (C.chr ((b - 0xC0) * 64 + more 1), 2)
  | b < 0xF0 = (C.chr ((b - 0xE0) * 4096 + more 1 * 64 + more 2), 3)
  | otherwise = (C.chr ((b - 0xF0) * 262144 + more 1 * 4096 + more 2 * 64 + more 3), 4)
  where
    b = byteAt a i
    more k = byteAt a (i + k) - 0x80

-- Where the character before a place starts.
before :: Bytes -> Int -> Int
before a i = if b >= 0x80 && b < 0xC0 then before a (i - 1) else i - 1
  where
    b = byteAt a (i - 1)

-- A character's bytes written at a place.
write :: Integer -> Int -> Char -> IO ()
write a i c
  | n < 0x80 = put 0 n
  | n < 0x800 = put 0 (0xC0 + n `div` 64) >> put 1 (0x80 + n `mod` 64)
  | n < 0x10000 = put 0 (0xE0 + n `div` 4096) >> put 1 (0x80 + n `div` 64 `mod` 64) >> put 2 (0x80 + n `mod` 64)
  | otherwise = put 0 (0xF0 + n `div` 262144) >> put 1 (0x80 + n `div` 4096 `mod` 64) >> put 2 (0x80 + n `div` 64 `mod` 64) >> put 3 (0x80 + n `mod` 64)
  where
    n = C.ord c
    put k b = primBytesWrite a (toInteger (i + k)) (toInteger b)

pack :: String -> Text
pack s = made (P.sum (P.map width s')) (\a -> go a 0 s')
  where
    s' = P.map sound s
    go _ _ [] = return ()
    go a i (c : cs) = write a i c >> go a (i + width c) cs

unpack :: Text -> String
unpack (Text a off len) = go off
  where
    end = off + len
    go i
      | i >= end = []
      | otherwise = case charAt a i of
          (c, w) -> c : go (i + w)

instance Eq Text where
  Text a i n == Text b j m = n == m && ordered a i b j n == 0

instance Ord Text where
  -- The bytes of UTF-8 are in the order of the characters.
  compare (Text a i n) (Text b j m)
    | first < 0 = LT
    | first > 0 = GT
    | otherwise = compare n m
    where
      first = ordered a i b j (min n m)

instance Show Text where
  showsPrec d t = showsPrec d (unpack t)

instance Read Text where
  readsPrec d s = [(pack x, rest) | (x, rest) <- readsPrec d s]

instance Semigroup Text where
  (<>) = append

instance Monoid Text where
  mempty = empty
  mconcat = concat

instance IsString Text where
  fromString = pack

-- A value as it is shown, as text.
show :: Show a => a -> Text
show = pack . P.show

empty :: Text
empty = primPerformIO (primBytesNew 0 >>= \a -> return (Text (Bytes a) 0 0))

singleton :: Char -> Text
singleton c = pack [c]

-- The same text in bytes of its own, and no more of them than it is.
copy :: Text -> Text
copy t@(Text _ _ len) = made len (\a -> copied a 0 t)

append :: Text -> Text -> Text
append t1@(Text _ _ n) t2@(Text _ _ m)
  | n == 0 = t2
  | m == 0 = t1
  | otherwise = made (n + m) (\a -> copied a 0 t1 >> copied a n t2)

concat :: [Text] -> Text
concat ts = case P.filter (not . null) ts of
  [] -> empty
  [t] -> t
  some -> made (P.sum [n | Text _ _ n <- some]) (\a -> go a 0 some)
  where
    go _ _ [] = return ()
    go a i (t@(Text _ _ n) : rest) = copied a i t >> go a (i + n) rest

cons :: Char -> Text -> Text
cons c t = append (singleton c) t

snoc :: Text -> Char -> Text
snoc t c = append t (singleton c)

uncons :: Text -> Maybe (Char, Text)
uncons (Text a off len)
  | len == 0 = Nothing
  | otherwise = case charAt a off of
      (c, w) -> Just (c, Text a (off + w) (len - w))

unsnoc :: Text -> Maybe (Text, Char)
unsnoc (Text a off len)
  | len == 0 = Nothing
  | otherwise = let start = before a (off + len) in Just (Text a off (start - off), fst (charAt a start))

head :: Text -> Char
head t = maybe (error "Data.Text.head: empty input") fst (uncons t)

last :: Text -> Char
last t = maybe (error "Data.Text.last: empty input") snd (unsnoc t)

tail :: Text -> Text
tail t = maybe (error "Data.Text.tail: empty input") snd (uncons t)

init :: Text -> Text
init t = maybe (error "Data.Text.init: empty input") fst (unsnoc t)

null :: Text -> Bool
null (Text _ _ len) = len == 0

-- How many characters, which is found by going through the bytes.
length :: Text -> Int
length (Text a off len) = go off 0
  where
    end = off + len
    go i n
      | i >= end = n
      | otherwise = go (i + widthFrom (byteAt a i)) (n + 1)

compareLength :: Text -> Int -> Ordering
compareLength t n = compare (length t) n

-- How many bytes the first so many characters are; and the last.
bytesOf :: Int -> Text -> Int
bytesOf n (Text a off len) = go off n
  where
    end = off + len
    go i k
      | k <= 0 || i >= end = i - off
      | otherwise = go (i + widthFrom (byteAt a i)) (k - 1)

bytesOfEnd :: Int -> Text -> Int
bytesOfEnd n (Text a off len) = go (off + len) n
  where
    go i k
      | k <= 0 || i <= off = off + len - i
      | otherwise = go (before a i) (k - 1)

-- How many bytes at the front are characters of some kind; and at the end.
bytesWhile :: (Char -> Bool) -> Text -> Int
bytesWhile p (Text a off len) = go off
  where
    end = off + len
    go i
      | i >= end = len
      | otherwise = case charAt a i of
          (c, w) -> if p c then go (i + w) else i - off

bytesWhileEnd :: (Char -> Bool) -> Text -> Int
bytesWhileEnd p (Text a off len) = go (off + len)
  where
    go i
      | i <= off = len
      | otherwise = let start = before a i in if p (fst (charAt a start)) then go start else off + len - i

-- A text's first so many bytes, and the rest: the same bytes.
front, back :: Int -> Text -> Text
front k (Text a off _) = Text a off k
back k (Text a off len) = Text a (off + k) (len - k)

take :: Int -> Text -> Text
take n t = front (bytesOf n t) t

drop :: Int -> Text -> Text
drop n t = back (bytesOf n t) t

takeEnd :: Int -> Text -> Text
takeEnd n t@(Text _ _ len) = back (len - bytesOfEnd n t) t

dropEnd :: Int -> Text -> Text
dropEnd n t@(Text _ _ len) = front (len - bytesOfEnd n t) t

splitAt :: Int -> Text -> (Text, Text)
splitAt n t = let k = bytesOf n t in (front k t, back k t)

takeWhile :: (Char -> Bool) -> Text -> Text
takeWhile p t = front (bytesWhile p t) t

dropWhile :: (Char -> Bool) -> Text -> Text
dropWhile p t = back (bytesWhile p t) t

takeWhileEnd :: (Char -> Bool) -> Text -> Text
takeWhileEnd p t@(Text _ _ len) = back (len - bytesWhileEnd p t) t

dropWhileEnd :: (Char -> Bool) -> Text -> Text
dropWhileEnd p t@(Text _ _ len) = front (len - bytesWhileEnd p t) t

dropAround :: (Char -> Bool) -> Text -> Text
dropAround p = dropWhile p . dropWhileEnd p

strip :: Text -> Text
strip = dropAround C.isSpace

stripStart :: Text -> Text
stripStart = dropWhile C.isSpace

stripEnd :: Text -> Text
stripEnd = dropWhileEnd C.isSpace

span :: (Char -> Bool) -> Text -> (Text, Text)
span p t = let k = bytesWhile p t in (front k t, back k t)

break :: (Char -> Bool) -> Text -> (Text, Text)
break p = span (not . p)

-- One text in another.

-- Where one text is in another, in bytes from the other's start, no two
-- of the places sharing any.
places :: Text -> Text -> [Int]
places (Text na no nl) (Text ha ho hl) = go 0
  where
    go i
      | i + nl > hl = []
      | ordered na no ha (ho + i) nl == 0 = i : go (i + nl)
      | otherwise = go (i + 1)

needle :: String -> Text -> Text
needle what t = if null t then error ("Data.Text." ++ what ++ ": empty input") else t

isPrefixOf :: Text -> Text -> Bool
isPrefixOf (Text a i n) (Text b j m) = n <= m && ordered a i b j n == 0

isSuffixOf :: Text -> Text -> Bool
isSuffixOf (Text a i n) (Text b j m) = n <= m && ordered a i b (j + m - n) n == 0

isInfixOf :: Text -> Text -> Bool
isInfixOf n h = null n || not (P.null (places n h))

stripPrefix :: Text -> Text -> Maybe Text
stripPrefix p@(Text _ _ n) t = if isPrefixOf p t then Just (back n t) else Nothing

stripSuffix :: Text -> Text -> Maybe Text
stripSuffix s@(Text _ _ n) t@(Text _ _ m) = if isSuffixOf s t then Just (front (m - n) t) else Nothing

-- Up to the first place one text is in another, and from there.
breakOn :: Text -> Text -> (Text, Text)
breakOn n h = case places (needle "breakOn" n) h of
  i : _ -> (front i h, back i h)
  [] -> (h, empty)

-- Through the last place one text is in another, and what is after.
breakOnEnd :: Text -> Text -> (Text, Text)
breakOnEnd n h = case breakOn (reverse (needle "breakOnEnd" n)) (reverse h) of
  (after, through) -> (reverse through, reverse after)

-- Each place one text is in another: what is before it, and from there.
breakOnAll :: Text -> Text -> [(Text, Text)]
breakOnAll n h = [(front i h, back i h) | i <- places (needle "breakOnAll" n) h]

-- What is between the places one text is in another.
splitOn :: Text -> Text -> [Text]
splitOn n@(Text _ _ nl) h@(Text a off len) = go 0 (places (needle "splitOn" n) h)
  where
    go from [] = [Text a (off + from) (len - from)]
    go from (i : rest) = Text a (off + from) (i - from) : go (i + nl) rest

count :: Text -> Text -> Int
count n h = P.length (places (needle "count" n) h)

-- Every place one text is in another, with a third in its place.
replace :: Text -> Text -> Text -> Text
replace n with h = intercalate with (splitOn (needle "replace" n) h)

-- What is between the characters that are of some kind.
split :: (Char -> Bool) -> Text -> [Text]
split p t = case break p t of
  (piece, rest)
    | null rest -> [piece]
    | otherwise -> piece : split p (tail rest)

chunksOf :: Int -> Text -> [Text]
chunksOf n t
  | n <= 0 || null t = []
  | otherwise = case splitAt n t of
      (chunk, rest) -> chunk : chunksOf n rest

lines :: Text -> [Text]
lines t
  | null t = []
  | otherwise = case break (== '\n') t of
      (line, rest) -> line : (if null rest then [] else lines (tail rest))

words :: Text -> [Text]
words t = case break C.isSpace (dropWhile C.isSpace t) of
  (word, rest)
    | null word -> []
    | otherwise -> word : words rest

unlines :: [Text] -> Text
unlines ts = concat (P.concatMap (\t -> [t, singleton '\n']) ts)

unwords :: [Text] -> Text
unwords = intercalate (singleton ' ')

intercalate :: Text -> [Text] -> Text
intercalate sep ts = concat (L.intersperse sep ts)

group :: Text -> [Text]
group = groupBy (==)

groupBy :: (Char -> Char -> Bool) -> Text -> [Text]
groupBy same t@(Text a off len)
  | len == 0 = []
  | otherwise = case charAt a off of
      (c, w) -> let k = w + bytesWhile (same c) (back w t) in front k t : groupBy same (back k t)

inits :: Text -> [Text]
inits t@(Text a off len) = go off
  where
    end = off + len
    go i = front (i - off) t : (if i >= end then [] else go (i + widthFrom (byteAt a i)))

tails :: Text -> [Text]
tails t = t : (if null t then [] else tails (tail t))

-- What two texts start with alike, if anything, and the rest of each.
commonPrefixes :: Text -> Text -> Maybe (Text, Text, Text)
commonPrefixes t1 t2 = if k == 0 then Nothing else Just (front k t1, back k t1, back k t2)
  where
    k = P.sum (P.map (width . fst) (P.takeWhile (uncurry (==)) (P.zip (unpack t1) (unpack t2))))

replicate :: Int -> Text -> Text
replicate n t@(Text _ _ len)
  | n <= 0 || len == 0 = empty
  | otherwise = made (n * len) (\a -> mapM_ (\k -> copied a (k * len) t) [0 .. n - 1])

-- A character at a time.

through :: (String -> String) -> Text -> Text
through f = pack . f . unpack

map :: (Char -> Char) -> Text -> Text
map f = through (P.map f)

filter :: (Char -> Bool) -> Text -> Text
filter p = through (P.filter p)

reverse :: Text -> Text
reverse = through P.reverse

intersperse :: Char -> Text -> Text
intersperse c = through (L.intersperse c)

concatMap :: (Char -> Text) -> Text -> Text
concatMap f t = concat (P.map f (unpack t))

transpose :: [Text] -> [Text]
transpose = P.map pack . L.transpose . P.map unpack

zipWith :: (Char -> Char -> Char) -> Text -> Text -> Text
zipWith f t1 t2 = pack (P.zipWith f (unpack t1) (unpack t2))

zip :: Text -> Text -> [(Char, Char)]
zip t1 t2 = P.zip (unpack t1) (unpack t2)

partition :: (Char -> Bool) -> Text -> (Text, Text)
partition p t = (filter p t, filter (not . p) t)

-- As small letters, for comparing without regard to case. Of the letters
-- that fold to more than one, only the sharp s is known here.
toCaseFold :: Text -> Text
toCaseFold = through (P.concatMap (\c -> if c == '\223' then "ss" else [C.toLower c]))

toLower :: Text -> Text
toLower = map C.toLower

toUpper :: Text -> Text
toUpper = map C.toUpper

-- The first letter of each word a capital, and the rest small. A word is
-- what is between spaces.
toTitle :: Text -> Text
toTitle = through (go True)
  where
    go _ [] = []
    go start (c : rest)
      | C.isLetter c = (if start then C.toUpper c else C.toLower c) : go False rest
      | C.isSpace c = c : go True rest
      | otherwise = c : go start rest

padding :: Int -> Char -> Text
padding n c = pack (P.replicate n c)

justifyLeft :: Int -> Char -> Text -> Text
justifyLeft n c t = append t (padding (n - length t) c)

justifyRight :: Int -> Char -> Text -> Text
justifyRight n c t = append (padding (n - length t) c) t

center :: Int -> Char -> Text -> Text
center n c t
  | missing <= 0 = t
  | otherwise = concat [padding left c, t, padding right c]
  where
    missing = n - length t
    right = missing `div` 2
    left = missing - right

foldl :: (a -> Char -> a) -> a -> Text -> a
foldl f z = P.foldl f z . unpack

foldl' :: (a -> Char -> a) -> a -> Text -> a
foldl' f z = L.foldl' f z . unpack

foldr :: (Char -> a -> a) -> a -> Text -> a
foldr f z = P.foldr f z . unpack

any :: (Char -> Bool) -> Text -> Bool
any p = P.any p . unpack

all :: (Char -> Bool) -> Text -> Bool
all p = P.all p . unpack

maximum :: Text -> Char
maximum = P.maximum . unpack

minimum :: Text -> Char
minimum = P.minimum . unpack

find :: (Char -> Bool) -> Text -> Maybe Char
find p = L.find p . unpack

elem :: Char -> Text -> Bool
elem c = P.elem c . unpack

index :: Text -> Int -> Char
index t n = unpack t !! n

findIndex :: (Char -> Bool) -> Text -> Maybe Int
findIndex p = L.findIndex p . unpack
