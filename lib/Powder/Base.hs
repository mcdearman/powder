-- Everything the prelude and the libraries beside it are made of, written
-- in the language the compilers accept. The modules a program imports
-- are selections from this one: Prelude, Data.List, Data.Char and the
-- rest each export what the Report says they do.
module Powder.Base where

-- Nothing from the prelude, which is made from this.
import Prelude ()

infixr 9 .
infixl 9 !!
infixr 8 ^, ^^, **
infixl 7 *, /, %, `quot`, `rem`, `div`, `mod`
infixl 6 +, -
infixr 6 <>
infixr 5 :, ++, :|
infix 4 ==, /=, <, <=, >, >=, `elem`, `notElem`
infixl 4 <$>, <$, <*>, *>, <*
infixr 3 &&
infixl 3 <|>
infixr 2 ||
infixl 1 >>, >>=
infixr 1 =<<
infixr 0 $, $!, `seq`

type String = [Char]

type ShowS = String -> String

data Maybe a = Nothing | Just a deriving (Eq, Ord, Show)

data Either a b = Left a | Right b deriving (Eq, Ord, Show)

data Ordering = LT | EQ | GT deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- Classes -----------------------------------------------------------------

class Eq a where
  (==), (/=) :: a -> a -> Bool
  x /= y = not (x == y)
  x == y = not (x /= y)

class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<), (<=), (>), (>=) :: a -> a -> Bool
  max, min :: a -> a -> a
  compare x y = if x == y then EQ else if x <= y then LT else GT
  x < y = case compare x y of
    LT -> True
    _ -> False
  x <= y = case compare x y of
    GT -> False
    _ -> True
  x > y = case compare x y of
    GT -> True
    _ -> False
  x >= y = case compare x y of
    LT -> False
    _ -> True
  max x y = if x <= y then y else x
  min x y = if x <= y then x else y

class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS
  showsPrec _ x s = show x ++ s
  show x = showsPrec 0 x ""
  showList = showListWith shows

class Num a where
  (+), (-), (*) :: a -> a -> a
  negate, abs, signum :: a -> a
  fromInteger :: Integer -> a
  x - y = x + negate y
  negate x = 0 - x

class (Num a, Ord a) => Real a where
  toRational :: a -> Rational

class Enum a where
  succ, pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]
  succ x = toEnum (fromEnum x + 1)
  pred x = toEnum (fromEnum x - 1)
  enumFrom x = map toEnum (enumFrom (fromEnum x))
  enumFromTo x y = map toEnum (enumFromTo (fromEnum x) (fromEnum y))
  enumFromThen x y = map toEnum (enumFromThen (fromEnum x) (fromEnum y))
  enumFromThenTo x y z = map toEnum (enumFromThenTo (fromEnum x) (fromEnum y) (fromEnum z))

class (Real a, Enum a) => Integral a where
  quot, rem, div, mod :: a -> a -> a
  quotRem, divMod :: a -> a -> (a, a)
  toInteger :: a -> Integer
  quot x y = fst (quotRem x y)
  rem x y = snd (quotRem x y)
  quotRem x y = (quot x y, rem x y)
  div x y = if rem x y * y < 0 then quot x y - 1 else quot x y
  mod x y = let r = rem x y in if r * y < 0 then r + y else r
  divMod x y = (div x y, mod x y)

class Num a => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a
  recip x = 1 / x
  x / y = x * recip y

class Fractional a => Floating a where
  pi :: a
  exp, log, sqrt :: a -> a
  (**), logBase :: a -> a -> a
  sin, cos, tan, asin, acos, atan :: a -> a
  sinh, cosh, tanh, asinh, acosh, atanh :: a -> a
  x ** y = exp (log x * y)
  logBase x y = log y / log x
  sqrt x = x ** 0.5
  tan x = sin x / cos x
  tanh x = sinh x / cosh x

class (Real a, Fractional a) => RealFrac a where
  properFraction :: Integral b => a -> (b, a)
  truncate, round, ceiling, floor :: Integral b => a -> b
  truncate x = fst (properFraction x)
  round x =
    let (n, r) = properFraction x
        m = if r < 0 then n - 1 else n + 1
        d = abs r - 0.5
     in if d < 0 then n else if d > 0 then m else if even n then n else m
  ceiling x = let (n, r) = properFraction x in if r > 0 then n + 1 else n
  floor x = let (n, r) = properFraction x in if r < 0 then n - 1 else n

class (RealFrac a, Floating a) => RealFloat a where
  floatRadix :: a -> Integer
  floatDigits :: a -> Int
  floatRange :: a -> (Int, Int)
  decodeFloat :: a -> (Integer, Int)
  encodeFloat :: Integer -> Int -> a
  exponent :: a -> Int
  significand :: a -> a
  scaleFloat :: Int -> a -> a
  isNaN, isInfinite, isDenormalized, isNegativeZero, isIEEE :: a -> Bool
  atan2 :: a -> a -> a
  exponent x = let (m, n) = decodeFloat x in if m == 0 then 0 else n + floatDigits x
  significand x = let (m, _) = decodeFloat x in encodeFloat m (negate (floatDigits x))
  scaleFloat k x = let (m, n) = decodeFloat x in encodeFloat m (n + k)

class Bounded a where
  minBound, maxBound :: a

class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  x <$ m = fmap (\_ -> x) m

-- The classes of actions are the ones programs are written against today,
-- not the Report's: a monad is an applicative functor, and what a monad
-- does when a pattern fails to match is a class of its own.
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
  fs <*> xs = liftA2 id fs xs
  liftA2 f x y = fmap f x <*> y
  a *> b = liftA2 (\_ y -> y) a b
  a <* b = liftA2 const a b

class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  m >> k = m >>= \_ -> k
  return = pure

class Monad m => MonadFail m where
  fail :: String -> m a

class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]
  many :: f a -> f [a]
  some v = liftA2 (:) v (many v)
  many v = some v <|> pure []

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

-- The types a string written in a program may be of, where
-- OverloadedStrings is on.
class IsString a where
  fromString :: String -> a

-- A list of characters is one, and of nothing else: said by way of a
-- class that only Char is in, so that a string written where a list is
-- wanted is a list of something, and that something is then found to be
-- Char. (GHC says it with an equality of types.)
class IsChar c where
  fromChar :: Char -> c

instance IsChar Char where
  fromChar c = c

instance IsChar c => IsString [c] where
  fromString = map fromChar

-- What can be folded: gone through an element at a time, in order. An
-- instance gives foldMap or foldr, and the rest come from that one.
class Foldable t where
  fold :: Monoid m => t m -> m
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr' :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b
  foldl' :: (b -> a -> b) -> b -> t a -> b
  foldr1 :: (a -> a -> a) -> t a -> a
  foldl1 :: (a -> a -> a) -> t a -> a
  toList :: t a -> [a]
  null :: t a -> Bool
  length :: t a -> Int
  elem :: Eq a => a -> t a -> Bool
  maximum :: Ord a => t a -> a
  minimum :: Ord a => t a -> a
  sum :: Num a => t a -> a
  product :: Num a => t a -> a
  fold = foldMap id
  foldMap f = foldr (\x acc -> f x <> acc) mempty
  foldr f z t = appEndo (foldMap (\x -> Endo (f x)) t) z
  foldr' f z t = listFoldl' (\acc x -> f x acc) z (reverse (toList t))
  foldl f z t = listFoldl f z (toList t)
  foldl' f z t = listFoldl' f z (toList t)
  foldr1 f t = listFoldr1 f (toList t)
  foldl1 f t = listFoldl1 f (toList t)
  toList = foldr (:) []
  null = foldr (\_ _ -> False) True
  length = foldl' (\n _ -> n + 1) 0
  elem x = foldr (\y acc -> x == y || acc) False
  maximum t = case toList t of
    [] -> error "maximum: empty structure"
    x : xs -> listFoldl' max x xs
  minimum t = case toList t of
    [] -> error "minimum: empty structure"
    x : xs -> listFoldl' min x xs
  sum = foldl' (+) 0
  product = foldl' (*) 1

-- The functions from a type to itself, under composition: what a fold
-- from the right is, when all there is to fold with is a monoid.
newtype Endo a = Endo {appEndo :: a -> a}

instance Semigroup (Endo a) where
  Endo f <> Endo g = Endo (\x -> f (g x))

instance Monoid (Endo a) where
  mempty = Endo (\x -> x)

-- What can be gone through with an action for each element, keeping its
-- shape.
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
  mapM :: Monad m => (a -> m b) -> t a -> m (t b)
  sequence :: Monad m => t (m a) -> m (t a)
  traverse f t = sequenceA (fmap f t)
  sequenceA = traverse id
  mapM = traverse
  sequence = mapM id

class Semigroup a where
  (<>) :: a -> a -> a
  sconcat :: NonEmpty a -> a
  stimes :: Integral b => b -> a -> a
  sconcat (a :| as) = go a as
    where
      go b (c : cs) = b <> go c cs
      go b [] = b
  stimes n x
    | n <= 0 = error "stimes: positive multiplier expected"
    | n == 1 = x
    | even n = let y = stimes (n `quot` 2) x in y <> y
    | otherwise = x <> stimes (n - 1) x

-- A list that has at least one element.
data NonEmpty a = a :| [a] deriving (Eq, Ord)

instance Show a => Show (NonEmpty a) where
  showsPrec d (x :| xs) = showParen (d > 5) (showsPrec 6 x . showString " :| " . showsPrec 6 xs)

instance Semigroup (NonEmpty a) where
  (a :| as) <> (b :| bs) = a :| (as ++ b : bs)

instance Functor NonEmpty where
  fmap f (a :| as) = f a :| map f as

instance Applicative NonEmpty where
  pure a = a :| []
  fs <*> xs = fs >>= \f -> fmap f xs

instance Monad NonEmpty where
  (a :| as) >>= f = case f a of
    b :| bs -> b :| (bs ++ concatMap (\x -> case f x of c :| cs -> c : cs) as)

instance Foldable NonEmpty where
  foldr f z (a :| as) = f a (listFoldr f z as)
  toList (a :| as) = a : as
  length (_ :| as) = 1 + length as
  null _ = False

instance Traversable NonEmpty where
  traverse f (a :| as) = liftA2 (:|) (f a) (traverse f as)

class Semigroup a => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
  mappend = (<>)
  mconcat = foldr mappend mempty

instance Semigroup Ordering where
  LT <> _ = LT
  EQ <> o = o
  GT <> _ = GT

instance Monoid Ordering where
  mempty = EQ

instance Semigroup () where
  _ <> _ = ()

instance Monoid () where
  mempty = ()

instance Semigroup [a] where
  (<>) = (++)

instance Monoid [a] where
  mempty = []
  mconcat = concat

instance Semigroup a => Semigroup (Maybe a) where
  Nothing <> m = m
  m <> Nothing = m
  Just x <> Just y = Just (x <> y)

instance Semigroup a => Monoid (Maybe a) where
  mempty = Nothing

instance Semigroup b => Semigroup (a -> b) where
  f <> g = \x -> f x <> g x

instance Monoid b => Monoid (a -> b) where
  mempty = \_ -> mempty

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (a, b) <> (c, d) = (a <> c, b <> d)

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (a, b, c) where
  (a, b, c) <> (d, e, f) = (a <> d, b <> e, c <> f)

instance (Monoid a, Monoid b, Monoid c) => Monoid (a, b, c) where
  mempty = (mempty, mempty, mempty)

instance Semigroup (Either a b) where
  Left _ <> b = b
  a <> _ = a

-- Integers ----------------------------------------------------------------

instance Eq Integer where
  (==) = primIntEq

instance Ord Integer where
  (<) = primIntLt
  (<=) = primIntLe
  compare x y = if primIntLt x y then LT else if primIntEq x y then EQ else GT

instance Show Integer where
  showsPrec p n r = if n < 0 && p > 6 then '(' : showInteger n (')' : r) else showInteger n r

instance Num Integer where
  (+) = primIntAdd
  (-) = primIntSub
  (*) = primIntMul
  negate x = primIntSub 0 x
  abs x = if x < 0 then negate x else x
  signum x = if x < 0 then negate 1 else if x == 0 then 0 else 1
  fromInteger x = x

instance Real Integer where
  toRational x = x :% 1

instance Enum Integer where
  succ x = x + 1
  pred x = x - 1
  toEnum = primCoerce
  fromEnum = primCoerce
  enumFrom n = n : enumFrom (n + 1)
  enumFromTo a b = if a > b then [] else a : enumFromTo (a + 1) b
  enumFromThen a b = a : enumFromThen b (b + b - a)
  enumFromThenTo a b c
    | b >= a = if a > c then [] else a : enumFromThenTo b (b + b - a) c
    | otherwise = if a < c then [] else a : enumFromThenTo b (b + b - a) c

instance Integral Integer where
  quot = primIntQuot
  rem = primIntRem
  toInteger x = x

-- An Int is an Integer kept within 64 bits: what goes past the end comes
-- round to the other.

fromInt :: Int -> Integer
fromInt = primCoerce

toInt :: Integer -> Int
toInt x = primCoerce (primIntNarrow x)

instance Eq Int where
  (==) = primCoerce primIntEq

instance Ord Int where
  (<) = primCoerce primIntLt
  (<=) = primCoerce primIntLe
  compare x y = compare (fromInt x) (fromInt y)

instance Show Int where
  showsPrec p n = showsPrec p (fromInt n)

instance Num Int where
  a + b = toInt (primIntAdd (fromInt a) (fromInt b))
  a - b = toInt (primIntSub (fromInt a) (fromInt b))
  a * b = toInt (primIntMul (fromInt a) (fromInt b))
  negate x = toInt (primIntSub 0 (fromInt x))
  abs x = if x < 0 then negate x else x
  signum x = if x < 0 then negate 1 else if x == 0 then 0 else 1
  fromInteger = toInt

instance Real Int where
  toRational x = fromInt x :% 1

instance Enum Int where
  succ x = x + 1
  pred x = x - 1
  toEnum x = x
  fromEnum x = x
  enumFrom n = enumFromTo n maxBound
  enumFromTo a b = primCoerce (enumFromTo (fromInt a) (fromInt b))
  enumFromThen a b = enumFromThenTo a b (if b >= a then maxBound else minBound)
  enumFromThenTo a b c = primCoerce (enumFromThenTo (fromInt a) (fromInt b) (fromInt c))

instance Integral Int where
  quot a b = toInt (primIntQuot (fromInt a) (fromInt b))
  rem a b = toInt (primIntRem (fromInt a) (fromInt b))
  toInteger = fromInt

instance Bounded Int where
  minBound = primCoerce (primIntSub 0 9223372036854775808)
  maxBound = 9223372036854775807

showInteger :: Integer -> ShowS
showInteger n r
  | n < 0 = '-' : showInteger (negate n) r
  | n < 10 = digit n : r
  | otherwise = showInteger (quot n 10) (digit (rem n 10) : r)
  where
    digit d = primIntToChar (48 + d)

subtract :: Num a => a -> a -> a
subtract x y = y - x

even, odd :: Integral a => a -> Bool
even n = rem n 2 == 0
odd n = not (even n)

gcd :: Integral a => a -> a -> a
gcd a b = if b == 0 then abs a else gcd b (rem a b)

lcm :: Integral a => a -> a -> a
lcm a b = if a == 0 || b == 0 then 0 else abs (quot a (gcd a b) * b)

(^) :: (Num a, Integral b) => a -> b -> a
x ^ n = if n == 0 then 1 else x * x ^ (n - 1)

fromIntegral :: (Integral a, Num b) => a -> b
fromIntegral x = fromInteger (toInteger x)

-- Ratios ------------------------------------------------------------------

-- A ratio is kept in lowest terms, with a denominator above zero.
data Ratio a = !a :% !a

type Rational = Ratio Integer

(%) :: Integral a => a -> a -> Ratio a
x % y
  | y == 0 = error "Ratio: zero denominator"
  | otherwise = let d = gcd x y * signum y in quot x d :% quot y d

numerator, denominator :: Ratio a -> a
numerator (x :% _) = x
denominator (_ :% y) = y

instance Integral a => Eq (Ratio a) where
  (x :% y) == (x' :% y') = x == x' && y == y'

instance Integral a => Ord (Ratio a) where
  (x :% y) <= (x' :% y') = x * y' <= x' * y
  (x :% y) < (x' :% y') = x * y' < x' * y

instance Integral a => Num (Ratio a) where
  (x :% y) + (x' :% y') = (x * y' + x' * y) % (y * y')
  (x :% y) - (x' :% y') = (x * y' - x' * y) % (y * y')
  (x :% y) * (x' :% y') = (x * x') % (y * y')
  negate (x :% y) = negate x :% y
  abs (x :% y) = abs x :% y
  signum (x :% _) = signum x :% 1
  fromInteger x = fromInteger x :% 1

instance Integral a => Real (Ratio a) where
  toRational (x :% y) = toInteger x :% toInteger y

instance Integral a => Fractional (Ratio a) where
  (x :% y) / (x' :% y') = (x * y') % (y * x')
  recip (x :% y) = y % x
  fromRational (x :% y) = fromInteger x % fromInteger y

instance Integral a => RealFrac (Ratio a) where
  properFraction (x :% y) = let q = quot x y in (fromInteger (toInteger q), (x - q * y) :% y)

instance (Read a, Integral a) => Read (Ratio a) where
  readsPrec p = readParen (p > 7) (\r -> [(x % y, u) | (x, s) <- readsPrec 8 r, ("%", t) <- lex s, (y, u) <- readsPrec 8 t])

instance Integral a => Enum (Ratio a) where
  succ x = x + 1
  pred x = x - 1
  toEnum n = fromInteger (toInteger n) :% 1
  fromEnum = truncate
  enumFrom = numericFrom
  enumFromThen = numericFromThen
  enumFromTo = numericFromTo
  enumFromThenTo = numericFromThenTo

instance Show a => Show (Ratio a) where
  showsPrec p (x :% y) = showParen (p > 7) (showsPrec 8 x . showString " % " . showsPrec 8 y)

-- Floating point ----------------------------------------------------------

instance Eq Double where
  (==) = primDoubleEq

instance Ord Double where
  (<) = primDoubleLt
  (<=) = primDoubleLe
  -- Each said for itself, since what is not a number is neither more
  -- nor less than anything.
  x > y = primDoubleLt y x
  x >= y = primDoubleLe y x

instance Show Double where
  showsPrec p x = showParen (p > 6 && (x < 0 || isNegativeZero x)) (showString (shownDouble x))

instance Num Double where
  (+) = primDoubleAdd
  (-) = primDoubleSub
  (*) = primDoubleMul
  -- Not zero less it: the negation of zero is a zero of the other sign.
  negate x = primDoubleMul (primIntToDouble (primIntSub 0 1)) x
  abs x = if x < 0 then negate x else x
  signum x = if x < 0 then negate 1 else if x > 0 then 1 else x
  fromInteger = primIntToDouble

instance Real Double where
  toRational x =
    let m = primDoubleMantissa x
        e = primDoubleExponent x
     in if e >= 0 then (m * 2 ^ e) :% 1 else m % (2 ^ negate e)

instance Fractional Double where
  (/) = primDoubleDiv
  fromRational (x :% y) = primDoubleFromRatio x y

instance Floating Double where
  pi = 3.141592653589793
  exp = primDoubleFun 0
  log = primDoubleFun 1
  sqrt = primDoubleFun 2
  sin = primDoubleFun 3
  cos = primDoubleFun 4
  tan = primDoubleFun 5
  asin = primDoubleFun 6
  acos = primDoubleFun 7
  atan = primDoubleFun 8
  sinh = primDoubleFun 9
  cosh = primDoubleFun 10
  tanh = primDoubleFun 11
  asinh = primDoubleFun 12
  acosh = primDoubleFun 13
  atanh = primDoubleFun 14
  (**) = primDoublePow

instance RealFrac Double where
  properFraction x = let n = primDoubleTruncate x in (fromInteger n, x - primIntToDouble n)

instance RealFloat Double where
  floatRadix _ = 2
  floatDigits _ = 53
  floatRange _ = (negate 1021, 1024)
  decodeFloat x = (primDoubleMantissa x, fromInteger (primDoubleExponent x))
  encodeFloat m n = primDoubleEncode m (toInteger n)
  isNaN x = not (x == x)
  isInfinite x = x == 1 / 0 || x == negate 1 / 0
  isDenormalized x = x /= 0 && abs x < 2.2250738585072014e-308
  isNegativeZero x = x == 0 && 1 / x < 0
  isIEEE _ = True
  atan2 = primDoubleAtan2

instance Enum Double where
  succ x = x + 1
  pred x = x - 1
  toEnum n = primIntToDouble (toInteger n)
  fromEnum = truncate
  enumFrom = numericFrom
  enumFromThen = numericFromThen
  enumFromTo = numericFromTo
  enumFromThenTo = numericFromThenTo

-- A Float is a Double kept to single precision: each result is the
-- nearest single-precision number to what a Double would have been.

toDouble :: Float -> Double
toDouble = primCoerce

toFloat :: Double -> Float
toFloat x = primCoerce (primFloatNarrow x)

float1 :: (Double -> Double) -> Float -> Float
float1 f x = toFloat (f (toDouble x))

float2 :: (Double -> Double -> Double) -> Float -> Float -> Float
float2 f x y = toFloat (f (toDouble x) (toDouble y))

instance Eq Float where
  (==) = primCoerce primDoubleEq

instance Ord Float where
  (<) = primCoerce primDoubleLt
  (<=) = primCoerce primDoubleLe
  x > y = toDouble x > toDouble y
  x >= y = toDouble x >= toDouble y

instance Show Float where
  showsPrec p x = showParen (p > 6 && (x < 0 || isNegativeZero x)) (showString (from 0))
    where
      from i = let c = primFloatShowAt (toDouble x) i in if c < 0 then [] else primIntToChar c : from (i + 1)

instance Num Float where
  (+) = float2 (+)
  (-) = float2 (-)
  (*) = float2 (*)
  negate = float1 negate
  abs = float1 abs
  signum = float1 signum
  fromInteger x = toFloat (primIntToDouble x)

instance Real Float where
  toRational x = toRational (toDouble x)

instance Fractional Float where
  (/) = float2 (/)
  fromRational q = toFloat (fromRational q)

instance Floating Float where
  pi = toFloat pi
  exp = float1 exp
  log = float1 log
  sqrt = float1 sqrt
  sin = float1 sin
  cos = float1 cos
  tan = float1 tan
  asin = float1 asin
  acos = float1 acos
  atan = float1 atan
  sinh = float1 sinh
  cosh = float1 cosh
  tanh = float1 tanh
  asinh = float1 asinh
  acosh = float1 acosh
  atanh = float1 atanh
  (**) = float2 (**)

instance RealFrac Float where
  properFraction x = let (n, r) = properFraction (toDouble x) in (n, toFloat r)

instance RealFloat Float where
  floatRadix _ = 2
  floatDigits _ = 24
  floatRange _ = (negate 125, 128)
  -- A single-precision number has 24 digits where a double has 53.
  decodeFloat x = let (m, e) = decodeFloat (toDouble x) in if m == 0 then (0, 0) else (quot m 536870912, e + 29)
  encodeFloat m n = toFloat (encodeFloat m n)
  isNaN x = isNaN (toDouble x)
  isInfinite x = isInfinite (toDouble x)
  isDenormalized x = x /= 0 && abs x < 1.17549435e-38
  isNegativeZero x = isNegativeZero (toDouble x)
  isIEEE _ = True
  atan2 = float2 atan2

instance Enum Float where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toFloat (toEnum n)
  fromEnum = truncate
  enumFrom = numericFrom
  enumFromThen = numericFromThen
  enumFromTo = numericFromTo
  enumFromThenTo = numericFromThenTo

-- A double as it is written, a character at a time.
shownDouble :: Double -> String
shownDouble x = from 0
  where
    from i = let c = primDoubleShowAt x i in if c < 0 then [] else primIntToChar c : from (i + 1)

-- Ranges of numbers with fractions, which run to half a step past the end.
numericFrom :: Fractional a => a -> [a]
numericFrom n = n : numericFrom (n + 1)

numericFromThen :: Fractional a => a -> a -> [a]
numericFromThen n m = n : numericFromThen m (m + m - n)

numericFromTo :: (Ord a, Fractional a) => a -> a -> [a]
numericFromTo n m = takeWhile (<= m + 1 / 2) (numericFrom n)

numericFromThenTo :: (Ord a, Fractional a) => a -> a -> a -> [a]
numericFromThenTo n n' m =
  let mid = (n' - n) / 2
   in takeWhile (if n' >= n then (<= m + mid) else (>= m + mid)) (numericFromThen n n')

realToFrac :: (Real a, Fractional b) => a -> b
realToFrac x = fromRational (toRational x)

(^^) :: (Fractional a, Integral b) => a -> b -> a
x ^^ n = if n >= 0 then x ^ n else recip (x ^ negate n)

-- Booleans ----------------------------------------------------------------

instance Eq Bool where
  True == True = True
  False == False = True
  _ == _ = False

instance Ord Bool where
  False <= _ = True
  True <= b = b

instance Show Bool where
  show True = "True"
  show False = "False"

instance Enum Bool where
  toEnum n = n /= 0
  fromEnum b = if b then 1 else 0
  enumFrom b = enumFromTo b True
  enumFromThen a b = enumFromThenTo a b (b >= a)

instance Bounded Bool where
  minBound = False
  maxBound = True

otherwise :: Bool
otherwise = True

not :: Bool -> Bool
not True = False
not False = True

(&&) :: Bool -> Bool -> Bool
True && x = x
False && _ = False

(||) :: Bool -> Bool -> Bool
True || _ = True
False || x = x

-- Functions ---------------------------------------------------------------

id :: a -> a
id x = x

const :: a -> b -> a
const x _ = x

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

($) :: (a -> b) -> a -> b
f $ x = f x

($!) :: (a -> b) -> a -> b
f $! x = x `seq` f x

seq :: a -> b -> b
seq = primSeq

until :: (a -> Bool) -> (a -> a) -> a -> a
until p f x = if p x then x else until p f (f x)

asTypeOf :: a -> a -> a
asTypeOf = const

error :: String -> a
error msg = primPerformIO (mapM_ (primPutChar 2) ("error: " ++ msg ++ "\n") >> leave 1)

undefined :: a
undefined = error "undefined"

-- Tuples, Maybe, Either ---------------------------------------------------

instance Eq () where
  _ == _ = True

instance Ord () where
  compare _ _ = EQ

instance Show () where
  show _ = "()"

-- Tuples -------------------------------------------------------------------

-- The unit, and tuples of up to fifteen, which is as far as the Report
-- says they must go.

instance Enum () where
  toEnum _ = ()
  fromEnum _ = 0
  enumFrom _ = [()]
  enumFromThen _ _ = repeat ()
  enumFromTo _ _ = [()]
  enumFromThenTo _ _ _ = repeat ()

instance Bounded () where
  minBound = ()
  maxBound = ()

instance (Eq t1, Eq t2) => Eq (t1, t2) where
  (a1, a2) == (b1, b2) = a1 == b1 && a2 == b2

instance (Ord t1, Ord t2) => Ord (t1, t2) where
  compare (a1, a2) (b1, b2) = case compare a1 b1 of { EQ -> compare a2 b2; unequal -> unequal }

instance (Bounded t1, Bounded t2) => Bounded (t1, t2) where
  minBound = (minBound, minBound)
  maxBound = (maxBound, maxBound)

instance (Show t1, Show t2) => Show (t1, t2) where
  showsPrec _ (a1, a2) = showChar '(' . shows a1 . showChar ',' . shows a2 . showChar ')'

instance (Read t1, Read t2) => Read (t1, t2) where
  readsPrec _ = readParen False (\r -> [((a1, a2), s2) | ("(", s0) <- lex r, (a1, u0) <- reads s0, (",", s1) <- lex u0, (a2, u1) <- reads s1, (")", s2) <- lex u1])

instance (Eq t1, Eq t2, Eq t3) => Eq (t1, t2, t3) where
  (a1, a2, a3) == (b1, b2, b3) = a1 == b1 && a2 == b2 && a3 == b3

instance (Ord t1, Ord t2, Ord t3) => Ord (t1, t2, t3) where
  compare (a1, a2, a3) (b1, b2, b3) = case compare a1 b1 of { EQ -> case compare a2 b2 of { EQ -> compare a3 b3; unequal -> unequal }; unequal -> unequal }

instance (Bounded t1, Bounded t2, Bounded t3) => Bounded (t1, t2, t3) where
  minBound = (minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound)

instance (Show t1, Show t2, Show t3) => Show (t1, t2, t3) where
  showsPrec _ (a1, a2, a3) = showChar '(' . shows a1 . showChar ',' . shows a2 . showChar ',' . shows a3 . showChar ')'

instance (Read t1, Read t2, Read t3) => Read (t1, t2, t3) where
  readsPrec _ = readParen False (\r -> [((a1, a2, a3), s3) | ("(", s0) <- lex r, (a1, u0) <- reads s0, (",", s1) <- lex u0, (a2, u1) <- reads s1, (",", s2) <- lex u1, (a3, u2) <- reads s2, (")", s3) <- lex u2])

instance (Eq t1, Eq t2, Eq t3, Eq t4) => Eq (t1, t2, t3, t4) where
  (a1, a2, a3, a4) == (b1, b2, b3, b4) = a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4

instance (Ord t1, Ord t2, Ord t3, Ord t4) => Ord (t1, t2, t3, t4) where
  compare (a1, a2, a3, a4) (b1, b2, b3, b4) = case compare a1 b1 of { EQ -> case compare a2 b2 of { EQ -> case compare a3 b3 of { EQ -> compare a4 b4; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }

instance (Bounded t1, Bounded t2, Bounded t3, Bounded t4) => Bounded (t1, t2, t3, t4) where
  minBound = (minBound, minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound, maxBound)

instance (Show t1, Show t2, Show t3, Show t4) => Show (t1, t2, t3, t4) where
  showsPrec _ (a1, a2, a3, a4) = showChar '(' . shows a1 . showChar ',' . shows a2 . showChar ',' . shows a3 . showChar ',' . shows a4 . showChar ')'

instance (Read t1, Read t2, Read t3, Read t4) => Read (t1, t2, t3, t4) where
  readsPrec _ = readParen False (\r -> [((a1, a2, a3, a4), s4) | ("(", s0) <- lex r, (a1, u0) <- reads s0, (",", s1) <- lex u0, (a2, u1) <- reads s1, (",", s2) <- lex u1, (a3, u2) <- reads s2, (",", s3) <- lex u2, (a4, u3) <- reads s3, (")", s4) <- lex u3])

instance (Eq t1, Eq t2, Eq t3, Eq t4, Eq t5) => Eq (t1, t2, t3, t4, t5) where
  (a1, a2, a3, a4, a5) == (b1, b2, b3, b4, b5) = a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4 && a5 == b5

instance (Ord t1, Ord t2, Ord t3, Ord t4, Ord t5) => Ord (t1, t2, t3, t4, t5) where
  compare (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5) = case compare a1 b1 of { EQ -> case compare a2 b2 of { EQ -> case compare a3 b3 of { EQ -> case compare a4 b4 of { EQ -> compare a5 b5; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }

instance (Bounded t1, Bounded t2, Bounded t3, Bounded t4, Bounded t5) => Bounded (t1, t2, t3, t4, t5) where
  minBound = (minBound, minBound, minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound)

instance (Show t1, Show t2, Show t3, Show t4, Show t5) => Show (t1, t2, t3, t4, t5) where
  showsPrec _ (a1, a2, a3, a4, a5) = showChar '(' . shows a1 . showChar ',' . shows a2 . showChar ',' . shows a3 . showChar ',' . shows a4 . showChar ',' . shows a5 . showChar ')'

instance (Read t1, Read t2, Read t3, Read t4, Read t5) => Read (t1, t2, t3, t4, t5) where
  readsPrec _ = readParen False (\r -> [((a1, a2, a3, a4, a5), s5) | ("(", s0) <- lex r, (a1, u0) <- reads s0, (",", s1) <- lex u0, (a2, u1) <- reads s1, (",", s2) <- lex u1, (a3, u2) <- reads s2, (",", s3) <- lex u2, (a4, u3) <- reads s3, (",", s4) <- lex u3, (a5, u4) <- reads s4, (")", s5) <- lex u4])

instance (Eq t1, Eq t2, Eq t3, Eq t4, Eq t5, Eq t6) => Eq (t1, t2, t3, t4, t5, t6) where
  (a1, a2, a3, a4, a5, a6) == (b1, b2, b3, b4, b5, b6) = a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4 && a5 == b5 && a6 == b6

instance (Ord t1, Ord t2, Ord t3, Ord t4, Ord t5, Ord t6) => Ord (t1, t2, t3, t4, t5, t6) where
  compare (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6) = case compare a1 b1 of { EQ -> case compare a2 b2 of { EQ -> case compare a3 b3 of { EQ -> case compare a4 b4 of { EQ -> case compare a5 b5 of { EQ -> compare a6 b6; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }

instance (Bounded t1, Bounded t2, Bounded t3, Bounded t4, Bounded t5, Bounded t6) => Bounded (t1, t2, t3, t4, t5, t6) where
  minBound = (minBound, minBound, minBound, minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound, maxBound)

instance (Show t1, Show t2, Show t3, Show t4, Show t5, Show t6) => Show (t1, t2, t3, t4, t5, t6) where
  showsPrec _ (a1, a2, a3, a4, a5, a6) = showChar '(' . shows a1 . showChar ',' . shows a2 . showChar ',' . shows a3 . showChar ',' . shows a4 . showChar ',' . shows a5 . showChar ',' . shows a6 . showChar ')'

instance (Read t1, Read t2, Read t3, Read t4, Read t5, Read t6) => Read (t1, t2, t3, t4, t5, t6) where
  readsPrec _ = readParen False (\r -> [((a1, a2, a3, a4, a5, a6), s6) | ("(", s0) <- lex r, (a1, u0) <- reads s0, (",", s1) <- lex u0, (a2, u1) <- reads s1, (",", s2) <- lex u1, (a3, u2) <- reads s2, (",", s3) <- lex u2, (a4, u3) <- reads s3, (",", s4) <- lex u3, (a5, u4) <- reads s4, (",", s5) <- lex u4, (a6, u5) <- reads s5, (")", s6) <- lex u5])

instance (Eq t1, Eq t2, Eq t3, Eq t4, Eq t5, Eq t6, Eq t7) => Eq (t1, t2, t3, t4, t5, t6, t7) where
  (a1, a2, a3, a4, a5, a6, a7) == (b1, b2, b3, b4, b5, b6, b7) = a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4 && a5 == b5 && a6 == b6 && a7 == b7

instance (Ord t1, Ord t2, Ord t3, Ord t4, Ord t5, Ord t6, Ord t7) => Ord (t1, t2, t3, t4, t5, t6, t7) where
  compare (a1, a2, a3, a4, a5, a6, a7) (b1, b2, b3, b4, b5, b6, b7) = case compare a1 b1 of { EQ -> case compare a2 b2 of { EQ -> case compare a3 b3 of { EQ -> case compare a4 b4 of { EQ -> case compare a5 b5 of { EQ -> case compare a6 b6 of { EQ -> compare a7 b7; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }

instance (Bounded t1, Bounded t2, Bounded t3, Bounded t4, Bounded t5, Bounded t6, Bounded t7) => Bounded (t1, t2, t3, t4, t5, t6, t7) where
  minBound = (minBound, minBound, minBound, minBound, minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound)

instance (Show t1, Show t2, Show t3, Show t4, Show t5, Show t6, Show t7) => Show (t1, t2, t3, t4, t5, t6, t7) where
  showsPrec _ (a1, a2, a3, a4, a5, a6, a7) = showChar '(' . shows a1 . showChar ',' . shows a2 . showChar ',' . shows a3 . showChar ',' . shows a4 . showChar ',' . shows a5 . showChar ',' . shows a6 . showChar ',' . shows a7 . showChar ')'

instance (Read t1, Read t2, Read t3, Read t4, Read t5, Read t6, Read t7) => Read (t1, t2, t3, t4, t5, t6, t7) where
  readsPrec _ = readParen False (\r -> [((a1, a2, a3, a4, a5, a6, a7), s7) | ("(", s0) <- lex r, (a1, u0) <- reads s0, (",", s1) <- lex u0, (a2, u1) <- reads s1, (",", s2) <- lex u1, (a3, u2) <- reads s2, (",", s3) <- lex u2, (a4, u3) <- reads s3, (",", s4) <- lex u3, (a5, u4) <- reads s4, (",", s5) <- lex u4, (a6, u5) <- reads s5, (",", s6) <- lex u5, (a7, u6) <- reads s6, (")", s7) <- lex u6])

instance (Eq t1, Eq t2, Eq t3, Eq t4, Eq t5, Eq t6, Eq t7, Eq t8) => Eq (t1, t2, t3, t4, t5, t6, t7, t8) where
  (a1, a2, a3, a4, a5, a6, a7, a8) == (b1, b2, b3, b4, b5, b6, b7, b8) = a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4 && a5 == b5 && a6 == b6 && a7 == b7 && a8 == b8

instance (Ord t1, Ord t2, Ord t3, Ord t4, Ord t5, Ord t6, Ord t7, Ord t8) => Ord (t1, t2, t3, t4, t5, t6, t7, t8) where
  compare (a1, a2, a3, a4, a5, a6, a7, a8) (b1, b2, b3, b4, b5, b6, b7, b8) = case compare a1 b1 of { EQ -> case compare a2 b2 of { EQ -> case compare a3 b3 of { EQ -> case compare a4 b4 of { EQ -> case compare a5 b5 of { EQ -> case compare a6 b6 of { EQ -> case compare a7 b7 of { EQ -> compare a8 b8; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }

instance (Bounded t1, Bounded t2, Bounded t3, Bounded t4, Bounded t5, Bounded t6, Bounded t7, Bounded t8) => Bounded (t1, t2, t3, t4, t5, t6, t7, t8) where
  minBound = (minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound)

instance (Show t1, Show t2, Show t3, Show t4, Show t5, Show t6, Show t7, Show t8) => Show (t1, t2, t3, t4, t5, t6, t7, t8) where
  showsPrec _ (a1, a2, a3, a4, a5, a6, a7, a8) = showChar '(' . shows a1 . showChar ',' . shows a2 . showChar ',' . shows a3 . showChar ',' . shows a4 . showChar ',' . shows a5 . showChar ',' . shows a6 . showChar ',' . shows a7 . showChar ',' . shows a8 . showChar ')'

instance (Read t1, Read t2, Read t3, Read t4, Read t5, Read t6, Read t7, Read t8) => Read (t1, t2, t3, t4, t5, t6, t7, t8) where
  readsPrec _ = readParen False (\r -> [((a1, a2, a3, a4, a5, a6, a7, a8), s8) | ("(", s0) <- lex r, (a1, u0) <- reads s0, (",", s1) <- lex u0, (a2, u1) <- reads s1, (",", s2) <- lex u1, (a3, u2) <- reads s2, (",", s3) <- lex u2, (a4, u3) <- reads s3, (",", s4) <- lex u3, (a5, u4) <- reads s4, (",", s5) <- lex u4, (a6, u5) <- reads s5, (",", s6) <- lex u5, (a7, u6) <- reads s6, (",", s7) <- lex u6, (a8, u7) <- reads s7, (")", s8) <- lex u7])

instance (Eq t1, Eq t2, Eq t3, Eq t4, Eq t5, Eq t6, Eq t7, Eq t8, Eq t9) => Eq (t1, t2, t3, t4, t5, t6, t7, t8, t9) where
  (a1, a2, a3, a4, a5, a6, a7, a8, a9) == (b1, b2, b3, b4, b5, b6, b7, b8, b9) = a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4 && a5 == b5 && a6 == b6 && a7 == b7 && a8 == b8 && a9 == b9

instance (Ord t1, Ord t2, Ord t3, Ord t4, Ord t5, Ord t6, Ord t7, Ord t8, Ord t9) => Ord (t1, t2, t3, t4, t5, t6, t7, t8, t9) where
  compare (a1, a2, a3, a4, a5, a6, a7, a8, a9) (b1, b2, b3, b4, b5, b6, b7, b8, b9) = case compare a1 b1 of { EQ -> case compare a2 b2 of { EQ -> case compare a3 b3 of { EQ -> case compare a4 b4 of { EQ -> case compare a5 b5 of { EQ -> case compare a6 b6 of { EQ -> case compare a7 b7 of { EQ -> case compare a8 b8 of { EQ -> compare a9 b9; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }

instance (Bounded t1, Bounded t2, Bounded t3, Bounded t4, Bounded t5, Bounded t6, Bounded t7, Bounded t8, Bounded t9) => Bounded (t1, t2, t3, t4, t5, t6, t7, t8, t9) where
  minBound = (minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound)

instance (Show t1, Show t2, Show t3, Show t4, Show t5, Show t6, Show t7, Show t8, Show t9) => Show (t1, t2, t3, t4, t5, t6, t7, t8, t9) where
  showsPrec _ (a1, a2, a3, a4, a5, a6, a7, a8, a9) = showChar '(' . shows a1 . showChar ',' . shows a2 . showChar ',' . shows a3 . showChar ',' . shows a4 . showChar ',' . shows a5 . showChar ',' . shows a6 . showChar ',' . shows a7 . showChar ',' . shows a8 . showChar ',' . shows a9 . showChar ')'

instance (Read t1, Read t2, Read t3, Read t4, Read t5, Read t6, Read t7, Read t8, Read t9) => Read (t1, t2, t3, t4, t5, t6, t7, t8, t9) where
  readsPrec _ = readParen False (\r -> [((a1, a2, a3, a4, a5, a6, a7, a8, a9), s9) | ("(", s0) <- lex r, (a1, u0) <- reads s0, (",", s1) <- lex u0, (a2, u1) <- reads s1, (",", s2) <- lex u1, (a3, u2) <- reads s2, (",", s3) <- lex u2, (a4, u3) <- reads s3, (",", s4) <- lex u3, (a5, u4) <- reads s4, (",", s5) <- lex u4, (a6, u5) <- reads s5, (",", s6) <- lex u5, (a7, u6) <- reads s6, (",", s7) <- lex u6, (a8, u7) <- reads s7, (",", s8) <- lex u7, (a9, u8) <- reads s8, (")", s9) <- lex u8])

instance (Eq t1, Eq t2, Eq t3, Eq t4, Eq t5, Eq t6, Eq t7, Eq t8, Eq t9, Eq t10) => Eq (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) where
  (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) == (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10) = a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4 && a5 == b5 && a6 == b6 && a7 == b7 && a8 == b8 && a9 == b9 && a10 == b10

instance (Ord t1, Ord t2, Ord t3, Ord t4, Ord t5, Ord t6, Ord t7, Ord t8, Ord t9, Ord t10) => Ord (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) where
  compare (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10) = case compare a1 b1 of { EQ -> case compare a2 b2 of { EQ -> case compare a3 b3 of { EQ -> case compare a4 b4 of { EQ -> case compare a5 b5 of { EQ -> case compare a6 b6 of { EQ -> case compare a7 b7 of { EQ -> case compare a8 b8 of { EQ -> case compare a9 b9 of { EQ -> compare a10 b10; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }

instance (Bounded t1, Bounded t2, Bounded t3, Bounded t4, Bounded t5, Bounded t6, Bounded t7, Bounded t8, Bounded t9, Bounded t10) => Bounded (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) where
  minBound = (minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound)

instance (Show t1, Show t2, Show t3, Show t4, Show t5, Show t6, Show t7, Show t8, Show t9, Show t10) => Show (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) where
  showsPrec _ (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = showChar '(' . shows a1 . showChar ',' . shows a2 . showChar ',' . shows a3 . showChar ',' . shows a4 . showChar ',' . shows a5 . showChar ',' . shows a6 . showChar ',' . shows a7 . showChar ',' . shows a8 . showChar ',' . shows a9 . showChar ',' . shows a10 . showChar ')'

instance (Read t1, Read t2, Read t3, Read t4, Read t5, Read t6, Read t7, Read t8, Read t9, Read t10) => Read (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) where
  readsPrec _ = readParen False (\r -> [((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10), s10) | ("(", s0) <- lex r, (a1, u0) <- reads s0, (",", s1) <- lex u0, (a2, u1) <- reads s1, (",", s2) <- lex u1, (a3, u2) <- reads s2, (",", s3) <- lex u2, (a4, u3) <- reads s3, (",", s4) <- lex u3, (a5, u4) <- reads s4, (",", s5) <- lex u4, (a6, u5) <- reads s5, (",", s6) <- lex u5, (a7, u6) <- reads s6, (",", s7) <- lex u6, (a8, u7) <- reads s7, (",", s8) <- lex u7, (a9, u8) <- reads s8, (",", s9) <- lex u8, (a10, u9) <- reads s9, (")", s10) <- lex u9])

instance (Eq t1, Eq t2, Eq t3, Eq t4, Eq t5, Eq t6, Eq t7, Eq t8, Eq t9, Eq t10, Eq t11) => Eq (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) where
  (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) == (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11) = a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4 && a5 == b5 && a6 == b6 && a7 == b7 && a8 == b8 && a9 == b9 && a10 == b10 && a11 == b11

instance (Ord t1, Ord t2, Ord t3, Ord t4, Ord t5, Ord t6, Ord t7, Ord t8, Ord t9, Ord t10, Ord t11) => Ord (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) where
  compare (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11) = case compare a1 b1 of { EQ -> case compare a2 b2 of { EQ -> case compare a3 b3 of { EQ -> case compare a4 b4 of { EQ -> case compare a5 b5 of { EQ -> case compare a6 b6 of { EQ -> case compare a7 b7 of { EQ -> case compare a8 b8 of { EQ -> case compare a9 b9 of { EQ -> case compare a10 b10 of { EQ -> compare a11 b11; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }

instance (Bounded t1, Bounded t2, Bounded t3, Bounded t4, Bounded t5, Bounded t6, Bounded t7, Bounded t8, Bounded t9, Bounded t10, Bounded t11) => Bounded (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) where
  minBound = (minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound)

instance (Show t1, Show t2, Show t3, Show t4, Show t5, Show t6, Show t7, Show t8, Show t9, Show t10, Show t11) => Show (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) where
  showsPrec _ (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = showChar '(' . shows a1 . showChar ',' . shows a2 . showChar ',' . shows a3 . showChar ',' . shows a4 . showChar ',' . shows a5 . showChar ',' . shows a6 . showChar ',' . shows a7 . showChar ',' . shows a8 . showChar ',' . shows a9 . showChar ',' . shows a10 . showChar ',' . shows a11 . showChar ')'

instance (Read t1, Read t2, Read t3, Read t4, Read t5, Read t6, Read t7, Read t8, Read t9, Read t10, Read t11) => Read (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) where
  readsPrec _ = readParen False (\r -> [((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11), s11) | ("(", s0) <- lex r, (a1, u0) <- reads s0, (",", s1) <- lex u0, (a2, u1) <- reads s1, (",", s2) <- lex u1, (a3, u2) <- reads s2, (",", s3) <- lex u2, (a4, u3) <- reads s3, (",", s4) <- lex u3, (a5, u4) <- reads s4, (",", s5) <- lex u4, (a6, u5) <- reads s5, (",", s6) <- lex u5, (a7, u6) <- reads s6, (",", s7) <- lex u6, (a8, u7) <- reads s7, (",", s8) <- lex u7, (a9, u8) <- reads s8, (",", s9) <- lex u8, (a10, u9) <- reads s9, (",", s10) <- lex u9, (a11, u10) <- reads s10, (")", s11) <- lex u10])

instance (Eq t1, Eq t2, Eq t3, Eq t4, Eq t5, Eq t6, Eq t7, Eq t8, Eq t9, Eq t10, Eq t11, Eq t12) => Eq (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) where
  (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) == (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12) = a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4 && a5 == b5 && a6 == b6 && a7 == b7 && a8 == b8 && a9 == b9 && a10 == b10 && a11 == b11 && a12 == b12

instance (Ord t1, Ord t2, Ord t3, Ord t4, Ord t5, Ord t6, Ord t7, Ord t8, Ord t9, Ord t10, Ord t11, Ord t12) => Ord (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) where
  compare (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12) = case compare a1 b1 of { EQ -> case compare a2 b2 of { EQ -> case compare a3 b3 of { EQ -> case compare a4 b4 of { EQ -> case compare a5 b5 of { EQ -> case compare a6 b6 of { EQ -> case compare a7 b7 of { EQ -> case compare a8 b8 of { EQ -> case compare a9 b9 of { EQ -> case compare a10 b10 of { EQ -> case compare a11 b11 of { EQ -> compare a12 b12; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }

instance (Bounded t1, Bounded t2, Bounded t3, Bounded t4, Bounded t5, Bounded t6, Bounded t7, Bounded t8, Bounded t9, Bounded t10, Bounded t11, Bounded t12) => Bounded (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) where
  minBound = (minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound)

instance (Show t1, Show t2, Show t3, Show t4, Show t5, Show t6, Show t7, Show t8, Show t9, Show t10, Show t11, Show t12) => Show (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) where
  showsPrec _ (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = showChar '(' . shows a1 . showChar ',' . shows a2 . showChar ',' . shows a3 . showChar ',' . shows a4 . showChar ',' . shows a5 . showChar ',' . shows a6 . showChar ',' . shows a7 . showChar ',' . shows a8 . showChar ',' . shows a9 . showChar ',' . shows a10 . showChar ',' . shows a11 . showChar ',' . shows a12 . showChar ')'

instance (Read t1, Read t2, Read t3, Read t4, Read t5, Read t6, Read t7, Read t8, Read t9, Read t10, Read t11, Read t12) => Read (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) where
  readsPrec _ = readParen False (\r -> [((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12), s12) | ("(", s0) <- lex r, (a1, u0) <- reads s0, (",", s1) <- lex u0, (a2, u1) <- reads s1, (",", s2) <- lex u1, (a3, u2) <- reads s2, (",", s3) <- lex u2, (a4, u3) <- reads s3, (",", s4) <- lex u3, (a5, u4) <- reads s4, (",", s5) <- lex u4, (a6, u5) <- reads s5, (",", s6) <- lex u5, (a7, u6) <- reads s6, (",", s7) <- lex u6, (a8, u7) <- reads s7, (",", s8) <- lex u7, (a9, u8) <- reads s8, (",", s9) <- lex u8, (a10, u9) <- reads s9, (",", s10) <- lex u9, (a11, u10) <- reads s10, (",", s11) <- lex u10, (a12, u11) <- reads s11, (")", s12) <- lex u11])

instance (Eq t1, Eq t2, Eq t3, Eq t4, Eq t5, Eq t6, Eq t7, Eq t8, Eq t9, Eq t10, Eq t11, Eq t12, Eq t13) => Eq (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) where
  (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) == (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13) = a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4 && a5 == b5 && a6 == b6 && a7 == b7 && a8 == b8 && a9 == b9 && a10 == b10 && a11 == b11 && a12 == b12 && a13 == b13

instance (Ord t1, Ord t2, Ord t3, Ord t4, Ord t5, Ord t6, Ord t7, Ord t8, Ord t9, Ord t10, Ord t11, Ord t12, Ord t13) => Ord (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) where
  compare (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13) = case compare a1 b1 of { EQ -> case compare a2 b2 of { EQ -> case compare a3 b3 of { EQ -> case compare a4 b4 of { EQ -> case compare a5 b5 of { EQ -> case compare a6 b6 of { EQ -> case compare a7 b7 of { EQ -> case compare a8 b8 of { EQ -> case compare a9 b9 of { EQ -> case compare a10 b10 of { EQ -> case compare a11 b11 of { EQ -> case compare a12 b12 of { EQ -> compare a13 b13; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }

instance (Bounded t1, Bounded t2, Bounded t3, Bounded t4, Bounded t5, Bounded t6, Bounded t7, Bounded t8, Bounded t9, Bounded t10, Bounded t11, Bounded t12, Bounded t13) => Bounded (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) where
  minBound = (minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound)

instance (Show t1, Show t2, Show t3, Show t4, Show t5, Show t6, Show t7, Show t8, Show t9, Show t10, Show t11, Show t12, Show t13) => Show (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) where
  showsPrec _ (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = showChar '(' . shows a1 . showChar ',' . shows a2 . showChar ',' . shows a3 . showChar ',' . shows a4 . showChar ',' . shows a5 . showChar ',' . shows a6 . showChar ',' . shows a7 . showChar ',' . shows a8 . showChar ',' . shows a9 . showChar ',' . shows a10 . showChar ',' . shows a11 . showChar ',' . shows a12 . showChar ',' . shows a13 . showChar ')'

instance (Read t1, Read t2, Read t3, Read t4, Read t5, Read t6, Read t7, Read t8, Read t9, Read t10, Read t11, Read t12, Read t13) => Read (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) where
  readsPrec _ = readParen False (\r -> [((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13), s13) | ("(", s0) <- lex r, (a1, u0) <- reads s0, (",", s1) <- lex u0, (a2, u1) <- reads s1, (",", s2) <- lex u1, (a3, u2) <- reads s2, (",", s3) <- lex u2, (a4, u3) <- reads s3, (",", s4) <- lex u3, (a5, u4) <- reads s4, (",", s5) <- lex u4, (a6, u5) <- reads s5, (",", s6) <- lex u5, (a7, u6) <- reads s6, (",", s7) <- lex u6, (a8, u7) <- reads s7, (",", s8) <- lex u7, (a9, u8) <- reads s8, (",", s9) <- lex u8, (a10, u9) <- reads s9, (",", s10) <- lex u9, (a11, u10) <- reads s10, (",", s11) <- lex u10, (a12, u11) <- reads s11, (",", s12) <- lex u11, (a13, u12) <- reads s12, (")", s13) <- lex u12])

instance (Eq t1, Eq t2, Eq t3, Eq t4, Eq t5, Eq t6, Eq t7, Eq t8, Eq t9, Eq t10, Eq t11, Eq t12, Eq t13, Eq t14) => Eq (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) where
  (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) == (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14) = a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4 && a5 == b5 && a6 == b6 && a7 == b7 && a8 == b8 && a9 == b9 && a10 == b10 && a11 == b11 && a12 == b12 && a13 == b13 && a14 == b14

instance (Ord t1, Ord t2, Ord t3, Ord t4, Ord t5, Ord t6, Ord t7, Ord t8, Ord t9, Ord t10, Ord t11, Ord t12, Ord t13, Ord t14) => Ord (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) where
  compare (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14) = case compare a1 b1 of { EQ -> case compare a2 b2 of { EQ -> case compare a3 b3 of { EQ -> case compare a4 b4 of { EQ -> case compare a5 b5 of { EQ -> case compare a6 b6 of { EQ -> case compare a7 b7 of { EQ -> case compare a8 b8 of { EQ -> case compare a9 b9 of { EQ -> case compare a10 b10 of { EQ -> case compare a11 b11 of { EQ -> case compare a12 b12 of { EQ -> case compare a13 b13 of { EQ -> compare a14 b14; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }

instance (Bounded t1, Bounded t2, Bounded t3, Bounded t4, Bounded t5, Bounded t6, Bounded t7, Bounded t8, Bounded t9, Bounded t10, Bounded t11, Bounded t12, Bounded t13, Bounded t14) => Bounded (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) where
  minBound = (minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound)

instance (Show t1, Show t2, Show t3, Show t4, Show t5, Show t6, Show t7, Show t8, Show t9, Show t10, Show t11, Show t12, Show t13, Show t14) => Show (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) where
  showsPrec _ (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = showChar '(' . shows a1 . showChar ',' . shows a2 . showChar ',' . shows a3 . showChar ',' . shows a4 . showChar ',' . shows a5 . showChar ',' . shows a6 . showChar ',' . shows a7 . showChar ',' . shows a8 . showChar ',' . shows a9 . showChar ',' . shows a10 . showChar ',' . shows a11 . showChar ',' . shows a12 . showChar ',' . shows a13 . showChar ',' . shows a14 . showChar ')'

instance (Read t1, Read t2, Read t3, Read t4, Read t5, Read t6, Read t7, Read t8, Read t9, Read t10, Read t11, Read t12, Read t13, Read t14) => Read (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) where
  readsPrec _ = readParen False (\r -> [((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14), s14) | ("(", s0) <- lex r, (a1, u0) <- reads s0, (",", s1) <- lex u0, (a2, u1) <- reads s1, (",", s2) <- lex u1, (a3, u2) <- reads s2, (",", s3) <- lex u2, (a4, u3) <- reads s3, (",", s4) <- lex u3, (a5, u4) <- reads s4, (",", s5) <- lex u4, (a6, u5) <- reads s5, (",", s6) <- lex u5, (a7, u6) <- reads s6, (",", s7) <- lex u6, (a8, u7) <- reads s7, (",", s8) <- lex u7, (a9, u8) <- reads s8, (",", s9) <- lex u8, (a10, u9) <- reads s9, (",", s10) <- lex u9, (a11, u10) <- reads s10, (",", s11) <- lex u10, (a12, u11) <- reads s11, (",", s12) <- lex u11, (a13, u12) <- reads s12, (",", s13) <- lex u12, (a14, u13) <- reads s13, (")", s14) <- lex u13])

instance (Eq t1, Eq t2, Eq t3, Eq t4, Eq t5, Eq t6, Eq t7, Eq t8, Eq t9, Eq t10, Eq t11, Eq t12, Eq t13, Eq t14, Eq t15) => Eq (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) where
  (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) == (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15) = a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4 && a5 == b5 && a6 == b6 && a7 == b7 && a8 == b8 && a9 == b9 && a10 == b10 && a11 == b11 && a12 == b12 && a13 == b13 && a14 == b14 && a15 == b15

instance (Ord t1, Ord t2, Ord t3, Ord t4, Ord t5, Ord t6, Ord t7, Ord t8, Ord t9, Ord t10, Ord t11, Ord t12, Ord t13, Ord t14, Ord t15) => Ord (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) where
  compare (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15) = case compare a1 b1 of { EQ -> case compare a2 b2 of { EQ -> case compare a3 b3 of { EQ -> case compare a4 b4 of { EQ -> case compare a5 b5 of { EQ -> case compare a6 b6 of { EQ -> case compare a7 b7 of { EQ -> case compare a8 b8 of { EQ -> case compare a9 b9 of { EQ -> case compare a10 b10 of { EQ -> case compare a11 b11 of { EQ -> case compare a12 b12 of { EQ -> case compare a13 b13 of { EQ -> case compare a14 b14 of { EQ -> compare a15 b15; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }; unequal -> unequal }

instance (Bounded t1, Bounded t2, Bounded t3, Bounded t4, Bounded t5, Bounded t6, Bounded t7, Bounded t8, Bounded t9, Bounded t10, Bounded t11, Bounded t12, Bounded t13, Bounded t14, Bounded t15) => Bounded (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) where
  minBound = (minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound)

instance (Show t1, Show t2, Show t3, Show t4, Show t5, Show t6, Show t7, Show t8, Show t9, Show t10, Show t11, Show t12, Show t13, Show t14, Show t15) => Show (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) where
  showsPrec _ (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = showChar '(' . shows a1 . showChar ',' . shows a2 . showChar ',' . shows a3 . showChar ',' . shows a4 . showChar ',' . shows a5 . showChar ',' . shows a6 . showChar ',' . shows a7 . showChar ',' . shows a8 . showChar ',' . shows a9 . showChar ',' . shows a10 . showChar ',' . shows a11 . showChar ',' . shows a12 . showChar ',' . shows a13 . showChar ',' . shows a14 . showChar ',' . shows a15 . showChar ')'

instance (Read t1, Read t2, Read t3, Read t4, Read t5, Read t6, Read t7, Read t8, Read t9, Read t10, Read t11, Read t12, Read t13, Read t14, Read t15) => Read (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) where
  readsPrec _ = readParen False (\r -> [((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15), s15) | ("(", s0) <- lex r, (a1, u0) <- reads s0, (",", s1) <- lex u0, (a2, u1) <- reads s1, (",", s2) <- lex u1, (a3, u2) <- reads s2, (",", s3) <- lex u2, (a4, u3) <- reads s3, (",", s4) <- lex u3, (a5, u4) <- reads s4, (",", s5) <- lex u4, (a6, u5) <- reads s5, (",", s6) <- lex u5, (a7, u6) <- reads s6, (",", s7) <- lex u6, (a8, u7) <- reads s7, (",", s8) <- lex u7, (a9, u8) <- reads s8, (",", s9) <- lex u8, (a10, u9) <- reads s9, (",", s10) <- lex u9, (a11, u10) <- reads s10, (",", s11) <- lex u10, (a12, u11) <- reads s11, (",", s12) <- lex u11, (a13, u12) <- reads s12, (",", s13) <- lex u12, (a14, u13) <- reads s13, (",", s14) <- lex u13, (a15, u14) <- reads s14, (")", s15) <- lex u14])

fst :: (a, b) -> a
fst (a, _) = a

snd :: (a, b) -> b
snd (_, b) = b

curry :: ((a, b) -> c) -> a -> b -> c
curry f a b = f (a, b)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)

instance Applicative Maybe where
  pure = Just
  Just f <*> m = fmap f m
  Nothing <*> _ = Nothing
  liftA2 f (Just x) (Just y) = Just (f x y)
  liftA2 _ _ _ = Nothing
  Just _ *> m = m
  Nothing *> _ = Nothing

instance Monad Maybe where
  Nothing >>= _ = Nothing
  Just x >>= k = k x

instance MonadFail Maybe where
  fail _ = Nothing

instance Alternative Maybe where
  empty = Nothing
  Nothing <|> m = m
  m <|> _ = m

instance Functor (Either e) where
  fmap _ (Left e) = Left e
  fmap f (Right x) = Right (f x)

instance Applicative (Either e) where
  pure = Right
  Left e <*> _ = Left e
  Right f <*> m = fmap f m

instance Monad (Either e) where
  Left e >>= _ = Left e
  Right x >>= k = k x

instance Functor ((->) r) where
  fmap f g = \x -> f (g x)

instance Applicative ((->) r) where
  pure x = \_ -> x
  f <*> g = \x -> f x (g x)
  liftA2 h f g = \x -> h (f x) (g x)

instance Monad ((->) r) where
  f >>= k = \x -> k (f x) x

instance Functor ((,) a) where
  fmap f (a, b) = (a, f b)

instance Monoid a => Applicative ((,) a) where
  pure x = (mempty, x)
  (u, f) <*> (v, x) = (u <> v, f x)

instance Monoid a => Monad ((,) a) where
  (u, a) >>= k = case k a of
    (v, b) -> (u <> v, b)

maybe :: b -> (a -> b) -> Maybe a -> b
maybe d _ Nothing = d
maybe _ f (Just x) = f x

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left a) = f a
either _ g (Right b) = g b

-- Lists -------------------------------------------------------------------

instance Eq a => Eq [a] where
  [] == [] = True
  (x : xs) == (y : ys) = x == y && xs == ys
  _ == _ = False

instance Ord a => Ord [a] where
  compare [] [] = EQ
  compare [] _ = LT
  compare _ [] = GT
  compare (x : xs) (y : ys) = case compare x y of
    EQ -> compare xs ys
    o -> o

instance Show a => Show [a] where
  showsPrec _ = showList

instance Functor [] where
  fmap = map

instance Applicative [] where
  pure x = [x]
  fs <*> xs = concatMap (\f -> map f xs) fs
  liftA2 f xs ys = concatMap (\x -> map (f x) ys) xs
  xs *> ys = concatMap (\_ -> ys) xs

instance Monad [] where
  xs >>= f = concatMap f xs

instance MonadFail [] where
  fail _ = []

instance Alternative [] where
  empty = []
  (<|>) = (++)

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x : xs) = if p x then x : filter p xs else filter p xs

-- The folds of a list, which are what Foldable's are for a list. They are
-- apart from the class so that each goes round without asking the class
-- again what a fold of a list is.
listFoldr :: (a -> b -> b) -> b -> [a] -> b
listFoldr _ z [] = z
listFoldr f z (x : xs) = f x (listFoldr f z xs)

listFoldr1 :: (a -> a -> a) -> [a] -> a
listFoldr1 _ [x] = x
listFoldr1 f (x : xs) = f x (listFoldr1 f xs)
listFoldr1 _ [] = error "foldr1: empty list"

listFoldl :: (b -> a -> b) -> b -> [a] -> b
listFoldl _ z [] = z
listFoldl f z (x : xs) = listFoldl f (f z x) xs

listFoldl1 :: (a -> a -> a) -> [a] -> a
listFoldl1 f (x : xs) = listFoldl f x xs
listFoldl1 _ [] = error "foldl1: empty list"

listFoldl' :: (b -> a -> b) -> b -> [a] -> b
listFoldl' _ z [] = z
listFoldl' f z (x : xs) = let z' = f z x in z' `seq` listFoldl' f z' xs

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl f z xs = z : case xs of
  [] -> []
  y : ys -> scanl f (f z y) ys

scanl1 :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x : xs) = scanl f x xs
scanl1 _ [] = []

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr _ z [] = [z]
scanr f z (x : xs) = case scanr f z xs of
  qs@(q : _) -> f x q : qs
  [] -> []

scanr1 :: (a -> a -> a) -> [a] -> [a]
scanr1 _ [] = []
scanr1 _ [x] = [x]
scanr1 f (x : xs) = case scanr1 f xs of
  qs@(q : _) -> f x q : qs
  [] -> []

zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 = zipWith3 (\a b c -> (a, b, c))

zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 f (a : as) (b : bs) (c : cs) = f a b c : zipWith3 f as bs cs
zipWith3 _ _ _ _ = []

unzip3 :: [(a, b, c)] -> ([a], [b], [c])
unzip3 = foldr (\(a, b, c) ~(as, bs, cs) -> (a : as, b : bs, c : cs)) ([], [], [])

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

concat :: Foldable t => t [a] -> [a]
concat = foldr (++) []

concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
concatMap f = foldr (\x acc -> f x ++ acc) []

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

and :: Foldable t => t Bool -> Bool
and = foldr (&&) True

or :: Foldable t => t Bool -> Bool
or = foldr (||) False

any :: Foldable t => (a -> Bool) -> t a -> Bool
any p = foldr (\x acc -> p x || acc) False

all :: Foldable t => (a -> Bool) -> t a -> Bool
all p = foldr (\x acc -> p x && acc) True

notElem :: (Foldable t, Eq a) => a -> t a -> Bool
notElem x xs = not (elem x xs)

find :: Foldable t => (a -> Bool) -> t a -> Maybe a
find p = foldr (\x rest -> if p x then Just x else rest) Nothing

maximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
maximumBy cmp = foldl1 (\x y -> case cmp x y of GT -> x; _ -> y)

minimumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
minimumBy cmp = foldl1 (\x y -> case cmp x y of GT -> y; _ -> x)

instance Foldable [] where
  foldMap f = listFoldr (\x acc -> f x <> acc) mempty
  foldr = listFoldr
  foldl = listFoldl
  foldl' = listFoldl'
  foldr1 = listFoldr1
  foldl1 = listFoldl1
  toList xs = xs
  null [] = True
  null _ = False
  length = listFoldl' (\n _ -> n + 1) 0
  elem x = listFoldr (\y acc -> x == y || acc) False
  sum = listFoldl' (+) 0
  product = listFoldl' (*) 1
  maximum (x : xs) = listFoldl' max x xs
  maximum [] = error "maximum: empty list"
  minimum (x : xs) = listFoldl' min x xs
  minimum [] = error "minimum: empty list"

instance Traversable [] where
  traverse f = listFoldr (\x ys -> liftA2 (:) (f x) ys) (pure [])
  mapM f = go
    where
      go [] = return []
      go (x : xs) = f x >>= \y -> go xs >>= \ys -> return (y : ys)

instance Foldable Maybe where
  foldMap _ Nothing = mempty
  foldMap f (Just x) = f x
  foldr _ z Nothing = z
  foldr f z (Just x) = f x z
  toList Nothing = []
  toList (Just x) = [x]

instance Traversable Maybe where
  traverse _ Nothing = pure Nothing
  traverse f (Just x) = fmap Just (f x)

instance Foldable (Either a) where
  foldMap _ (Left _) = mempty
  foldMap f (Right x) = f x
  foldr _ z (Left _) = z
  foldr f z (Right x) = f x z

instance Traversable (Either a) where
  traverse _ (Left e) = pure (Left e)
  traverse f (Right x) = fmap Right (f x)

instance Foldable ((,) a) where
  foldMap f (_, x) = f x
  foldr f z (_, x) = f x z

instance Traversable ((,) a) where
  traverse f (a, x) = fmap (\y -> (a, y)) (f x)

head :: [a] -> a
head (x : _) = x
head [] = error "head: empty list"

tail :: [a] -> [a]
tail (_ : xs) = xs
tail [] = error "tail: empty list"

last :: [a] -> a
last [x] = x
last (_ : xs) = last xs
last [] = error "last: empty list"

init :: [a] -> [a]
init [_] = []
init (x : xs) = x : init xs
init [] = error "init: empty list"

(!!) :: [a] -> Int -> a
(x : xs) !! n = if n == 0 then x else xs !! (n - 1)
[] !! _ = error "!!: index too large"

take :: Int -> [a] -> [a]
take n xs
  | n <= 0 = []
  | otherwise = case xs of
      [] -> []
      y : ys -> y : take (n - 1) ys

drop :: Int -> [a] -> [a]
drop n xs
  | n <= 0 = xs
  | otherwise = case xs of
      [] -> []
      _ : ys -> drop (n - 1) ys

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x : xs) = if p x then x : takeWhile p xs else []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p xs@(x : xs') = if p x then dropWhile p xs' else xs

span :: (a -> Bool) -> [a] -> ([a], [a])
span _ [] = ([], [])
span p xs@(x : xs')
  | p x = let (ys, zs) = span p xs' in (x : ys, zs)
  | otherwise = ([], xs)

break :: (a -> Bool) -> [a] -> ([a], [a])
break p = span (not . p)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (x : xs) (y : ys) = f x y : zipWith f xs ys
zipWith _ _ _ = []

zip :: [a] -> [b] -> [(a, b)]
zip = zipWith (\x y -> (x, y))

unzip :: [(a, b)] -> ([a], [b])
unzip = foldr (\(a, b) ~(as, bs) -> (a : as, b : bs)) ([], [])

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup _ [] = Nothing
lookup k ((k', v) : rest) = if k == k' then Just v else lookup k rest

replicate :: Int -> a -> [a]
replicate n x = take n (repeat x)

repeat :: a -> [a]
repeat x = let xs = x : xs in xs

cycle :: [a] -> [a]
cycle xs = let ys = xs ++ ys in ys

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

-- Characters and strings --------------------------------------------------

-- A character is its code when a program runs, but not to the type
-- checker: the primitives go between the two.

instance Eq Char where
  c == d = primIntEq (primCharToInt c) (primCharToInt d)

instance Ord Char where
  c < d = primIntLt (primCharToInt c) (primCharToInt d)
  c <= d = primIntLe (primCharToInt c) (primCharToInt d)
  compare c d = compare (primCharToInt c) (primCharToInt d)

instance Show Char where
  showsPrec _ '\'' = showString "'\\''"
  showsPrec _ c = showChar '\'' . showLitChar c . showChar '\''
  showList cs = showChar '"' . showl cs
    where
      showl "" = showChar '"'
      showl ('"' : rest) = showString "\\\"" . showl rest
      showl (c : rest) = showLitChar c . showl rest

instance Enum Char where
  toEnum = chr
  fromEnum = ord
  enumFrom c = enumFromTo c maxBound
  enumFromThen c d = enumFromThenTo c d (if d >= c then maxBound else minBound)

instance Bounded Char where
  minBound = chr 0
  maxBound = chr 1114111

-- A character as it is written inside a literal, quotes aside.
showLitChar :: Char -> ShowS
showLitChar c
  | ord c > 127 = showChar '\\' . protectEsc isDigit (shows (ord c))
  | ord c == 127 = showString "\\DEL"
  | c == '\\' = showString "\\\\"
  | ord c >= 32 = showChar c
  | c == '\a' = showString "\\a"
  | c == '\b' = showString "\\b"
  | c == '\f' = showString "\\f"
  | c == '\n' = showString "\\n"
  | c == '\r' = showString "\\r"
  | c == '\t' = showString "\\t"
  | c == '\v' = showString "\\v"
  | ord c == 14 = protectEsc (== 'H') (showString "\\SO")
  | otherwise = showString ('\\' : asciiTab !! ord c)

-- Keep what follows an escape from being read as more of it.
protectEsc :: (Char -> Bool) -> ShowS -> ShowS
protectEsc p f = f . cont
  where
    cont s@(c : _) | p c = "\\&" ++ s
    cont s = s

-- The names of the characters up to the space.
asciiTab :: [String]
asciiTab =
  [ "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", "BS", "HT", "LF", "VT", "FF", "CR", "SO", "SI",
    "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB", "CAN", "EM", "SUB", "ESC", "FS", "GS", "RS", "US", "SP"
  ]

-- What is left of each of two lists after what they start with in common.
match :: Eq a => [a] -> [a] -> ([a], [a])
match (x : xs) (y : ys) | x == y = match xs ys
match xs ys = (xs, ys)

-- A character of a literal as it is written, escape and all.
lexLitChar :: ReadS String
lexLitChar ('\\' : s) = map (prefix '\\') (lexEsc s)
  where
    lexEsc (c : rest) | c `elem` "abfnrtv\\\"'" = [([c], rest)]
    lexEsc ('^' : c : rest) | c >= '@' && c <= '_' = [(['^', c], rest)]
    lexEsc ('o' : rest) = [prefix 'o' (span isOctDigit rest)]
    lexEsc ('x' : rest) = [prefix 'x' (span isHexDigit rest)]
    lexEsc rest@(d : _) | isDigit d = [span isDigit rest]
    lexEsc rest@(c : _) | isUpper c = take 1 [(mne, after) | mne <- "DEL" : asciiTab, ([], after) <- [match mne rest]]
    lexEsc _ = []
    prefix c (t, rest) = (c : t, rest)
lexLitChar (c : s) = [([c], s)]
lexLitChar "" = []

-- The character that a character of a literal as written stands for.
readLitChar :: ReadS Char
readLitChar ('\\' : s) = readEsc s
  where
    readEsc ('a' : rest) = [('\a', rest)]
    readEsc ('b' : rest) = [('\b', rest)]
    readEsc ('f' : rest) = [('\f', rest)]
    readEsc ('n' : rest) = [('\n', rest)]
    readEsc ('r' : rest) = [('\r', rest)]
    readEsc ('t' : rest) = [('\t', rest)]
    readEsc ('v' : rest) = [('\v', rest)]
    readEsc ('\\' : rest) = [('\\', rest)]
    readEsc ('"' : rest) = [('"', rest)]
    readEsc ('\'' : rest) = [('\'', rest)]
    readEsc ('^' : c : rest) | c >= '@' && c <= '_' = [(chr (ord c - ord '@'), rest)]
    readEsc rest@(d : _) | isDigit d = [(chr n, after) | (n, after) <- readDec rest]
    readEsc ('o' : rest) = [(chr n, after) | (n, after) <- readOct rest]
    readEsc ('x' : rest) = [(chr n, after) | (n, after) <- readHex rest]
    readEsc rest@(c : _)
      | isUpper c = take 1 [(x, after) | (x, mne) <- (chr 127, "DEL") : zip (map chr (enumFrom 0)) asciiTab, ([], after) <- [match mne rest]]
    readEsc _ = []
readLitChar (c : s) = [(c, s)]
readLitChar "" = []

isOctDigit, isHexDigit :: Char -> Bool
isOctDigit c = c >= '0' && c <= '7'
isHexDigit c = isDigit c || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f'

ord :: Char -> Int
ord c = primCoerce (primCharToInt c)

chr :: Int -> Char
chr n = primIntToChar (primCoerce n)

-- Which of Unicode's general categories a character is in, by its place
-- in their order. Most questions about a character are asked of this,
-- after the common ones have been answered without it.
category :: Char -> Integer
category c = primCharCategory (primCharToInt c)

isDigit, isUpper, isLower, isAlpha, isAlphaNum, isSpace :: Char -> Bool
isDigit c = c >= '0' && c <= '9'
isUpper c = if ord c < 128 then c >= 'A' && c <= 'Z' else category c == 0 || category c == 2
isLower c = if ord c < 128 then c >= 'a' && c <= 'z' else category c == 1
isAlpha c = if ord c < 128 then c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' else category c <= 4
isAlphaNum c = if ord c < 128 then isAlpha c || isDigit c else category c <= 4 || category c >= 8 && category c <= 10
isSpace c = if ord c < 128 then c == ' ' || c >= '\t' && c <= '\r' else category c == 22

toUpper, toLower :: Char -> Char
toUpper c = if ord c < 128 then (if c >= 'a' && c <= 'z' then chr (ord c - 32) else c) else primIntToChar (primCharUpper (primCharToInt c))
toLower c = if ord c < 128 then (if c >= 'A' && c <= 'Z' then chr (ord c + 32) else c) else primIntToChar (primCharLower (primCharToInt c))

digitToInt :: Char -> Int
digitToInt c
  | isDigit c = ord c - ord '0'
  | c >= 'a' && c <= 'f' = ord c - ord 'a' + 10
  | c >= 'A' && c <= 'F' = ord c - ord 'A' + 10
  | otherwise = error "Char.digitToInt: not a digit"

shows :: Show a => a -> ShowS
shows = showsPrec 0

showChar :: Char -> ShowS
showChar = (:)

showString :: String -> ShowS
showString = (++)

showParen :: Bool -> ShowS -> ShowS
showParen b p = if b then showChar '(' . p . showChar ')' else p

showListWith :: (a -> ShowS) -> [a] -> ShowS
showListWith _ [] s = "[]" ++ s
showListWith showx (x : xs) s = '[' : showx x (rest xs)
  where
    rest [] = ']' : s
    rest (y : ys) = ',' : showx y (rest ys)

lines :: String -> [String]
lines [] = []
lines s = l : lines (drop 1 rest)
  where
    (l, rest) = span (/= '\n') s

unlines :: [String] -> String
unlines = concatMap (++ "\n")

words :: String -> [String]
words s = case dropWhile isSpace s of
  [] -> []
  s' -> w : words rest
    where
      (w, rest) = span (not . isSpace) s'

unwords :: [String] -> String
unwords [] = []
unwords [w] = w
unwords (w : ws) = w ++ ' ' : unwords ws

-- IO ----------------------------------------------------------------------

instance Functor IO where
  fmap f m = m >>= \x -> return (f x)

instance Applicative IO where
  pure = primReturn
  mf <*> m = mf >>= \f -> m >>= \x -> return (f x)
  liftA2 f m n = m >>= \x -> n >>= \y -> return (f x y)
  m *> n = m >>= \_ -> n
  m <* n = m >>= \x -> n >>= \_ -> return x

instance Monad IO where
  (>>=) = primBind
  return = primReturn

instance MonadFail IO where
  fail s = ioError (userError s)

instance Semigroup a => Semigroup (IO a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (IO a) where
  mempty = pure mempty

(=<<) :: Monad m => (a -> m b) -> m a -> m b
f =<< m = m >>= f

mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mapM_ f = foldr (\x rest -> f x >> rest) (return ())

sequence_ :: (Foldable t, Monad m) => t (m a) -> m ()
sequence_ = foldr (>>) (return ())

traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
traverse_ f = foldr (\x rest -> f x *> rest) (pure ())

sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequenceA_ = foldr (*>) (pure ())

when :: Applicative f => Bool -> f () -> f ()
when c m = if c then m else pure ()

unless :: Applicative f => Bool -> f () -> f ()
unless c m = if c then pure () else m

-- Cells ---------------------------------------------------------------------

-- A place for a value that can be changed, which the machine knows by a
-- number. What is kept is kept in a box, so that keeping it does not work
-- it out.
newtype Ref a = Ref Integer

data Box a = Box a

newRef :: a -> IO (Ref a)
newRef x = primCellNew (Box x) >>= \n -> return (Ref n)

readRef :: Ref a -> IO a
readRef (Ref n) = primCellRead n >>= \(Box x) -> return x

writeRef :: Ref a -> a -> IO ()
writeRef (Ref n) x = primCellWrite n (Box x)

-- An action given its own result, which it must not look at until it is
-- done.
fixIO :: (a -> IO a) -> IO a
fixIO f = do
  r <- newRef (error "fixIO: the result was needed before there was one")
  x <- f (primPerformIO (readRef r))
  writeRef r x
  return x

-- Leaving -------------------------------------------------------------------

-- What is to be done before the program ends, the last asked first.
exits :: Ref [IO ()]
exits = primPerformIO (newRef [])

atExit :: IO () -> IO ()
atExit act = readRef exits >>= \acts -> writeRef exits (act : acts)

-- Do what was left to be done at the end, once.
finish :: IO ()
finish = readRef exits >>= \acts -> writeRef exits [] >> sequence_ acts

-- End the program, with a status.
leave :: Integer -> IO a
leave n = finish >> primExit n

-- A program: its main, and then what was left to be done at the end.
top :: IO a -> IO a
top m = m >>= \x -> finish >> return x

-- I/O errors ----------------------------------------------------------------

type FilePath = String

data IOErrorType
  = AlreadyExists
  | NoSuchThing
  | ResourceBusy
  | ResourceExhausted
  | EOF
  | IllegalOperation
  | PermissionDenied
  | UserError
  | OtherError
  deriving (Eq, Ord)

instance Show IOErrorType where
  showsPrec _ t = showString $ case t of
    AlreadyExists -> "already exists"
    NoSuchThing -> "does not exist"
    ResourceBusy -> "resource busy"
    ResourceExhausted -> "resource exhausted"
    EOF -> "end of file"
    IllegalOperation -> "illegal operation"
    PermissionDenied -> "permission denied"
    UserError -> "user error"
    OtherError -> "failed"

-- What kind of thing went wrong, where, what is said of it, and the handle
-- and the file it went wrong with.
data IOError = IOError IOErrorType String String (Maybe Handle) (Maybe FilePath)

instance Eq IOError where
  IOError t l d h f == IOError t' l' d' h' f' = t == t' && l == l' && d == d' && h == h' && f == f'

instance Show IOError where
  showsPrec _ (IOError t place said h file) = subject . location . shows t . description
    where
      subject = case file of
        Just name -> showString name . showString ": "
        Nothing -> case h of
          Just handle -> shows handle . showString ": "
          Nothing -> id
      location = if null place then id else showString place . showString ": "
      description = if null said then id else showString " (" . showString said . showChar ')'

userError :: String -> IOError
userError s = IOError UserError "" s Nothing Nothing

-- One that nothing catches ends the program, saying what it was.
ioError :: IOError -> IO a
ioError e = primThrow (mapM_ (primPutChar 2) (show e ++ "\n") >> leave 1) e

catchIOError :: IO a -> (IOError -> IO a) -> IO a
catchIOError = primCatch

-- Handles -------------------------------------------------------------------

data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode deriving (Eq, Ord, Show, Read, Enum)

data BufferMode = NoBuffering | LineBuffering | BlockBuffering (Maybe Int) deriving (Eq, Ord, Show, Read)

data SeekMode = AbsoluteSeek | RelativeSeek | SeekFromEnd deriving (Eq, Ord, Show, Read, Enum)

-- A handle: the number the machine knows it by (0, 1 and 2 are the
-- standard ones), what it is called, and where its state is kept.
data Handle = Handle Integer String (Ref Opened)

-- A handle's state: whether it is open (0), semi-closed (1) or closed (2);
-- how it was opened; how it is said to be buffered; the code of a
-- character read from it and not yet taken, or -1; and whether what is
-- typed at it is echoed.
data Opened = Opened Int IOMode BufferMode Integer Bool

instance Eq Handle where
  Handle _ _ (Ref a) == Handle _ _ (Ref b) = a == b

instance Show Handle where
  showsPrec _ (Handle n name _) = if n < 3 then showString name else showString "{handle: " . showString name . showChar '}'

standard :: Integer -> String -> IOMode -> Handle
standard n name mode = Handle n name (primPerformIO (primIsTerminal n >>= \t -> newRef (Opened 0 mode (buffered t) (negate 1) True)))
  where
    buffered t = if n == 2 then NoBuffering else if t then LineBuffering else BlockBuffering Nothing

stdin, stdout, stderr :: Handle
stdin = standard 0 "<stdin>" ReadMode
stdout = standard 1 "<stdout>" WriteMode
stderr = standard 2 "<stderr>" WriteMode

-- The files that are open: each one's name, its handle's number, and
-- whether it is open for more than reading. A file has many readers or
-- one writer.
locks :: Ref [(FilePath, Integer, Bool)]
locks = primPerformIO (newRef [])

refuse :: IOErrorType -> String -> String -> Handle -> IO a
refuse t place said h = ioError (IOError t place said (Just h) Nothing)

-- A handle's state, where it is open.
wantOpen :: String -> Handle -> IO Opened
wantOpen place h@(Handle _ _ r) =
  readRef r >>= \s@(Opened phase _ _ _ _) ->
    if phase == 2
      then refuse IllegalOperation place "handle is closed" h
      else if phase == 1 then refuse IllegalOperation place "handle is semi-closed" h else return s

wantReadable :: String -> Handle -> IO Opened
wantReadable place h =
  wantOpen place h >>= \s@(Opened _ mode _ _ _) ->
    if mode == WriteMode || mode == AppendMode then refuse IllegalOperation place "handle is not open for reading" h else return s

-- For writing, a character read ahead is given back first, so that what
-- is written lands where the program has read to.
wantWritable :: String -> Handle -> IO ()
wantWritable place h@(Handle n _ r) =
  wantOpen place h >>= \(Opened phase mode buffering held echo) ->
    if mode == ReadMode
      then refuse IllegalOperation place "handle is not open for writing" h
      else
        if held < 0
          then return ()
          else primFileSeek n 1 (negate (width held)) >> writeRef r (Opened phase mode buffering (negate 1) echo)

-- How many bytes a character is in a file.
width :: Integer -> Integer
width c = if c < 0 then 0 else if c < 128 then 1 else if c < 2048 then 2 else if c < 65536 then 3 else 4

-- The code of the next character from what the machine has, or -1.
raw :: Integer -> IO Integer
raw n = if n == 0 then primGetChar else primHGetChar n

-- The code of a handle's next character, or -1 at the end.
next :: Handle -> IO Integer
next (Handle n _ r) =
  readRef r >>= \(Opened phase mode buffering held echo) ->
    if held < 0 then raw n else writeRef r (Opened phase mode buffering (negate 1) echo) >> return held

-- Close a handle for good: its file is free for others.
shut :: Handle -> IO ()
shut (Handle n _ r) = do
  Opened _ mode buffering _ echo <- readRef r
  writeRef r (Opened 2 mode buffering (negate 1) echo)
  if n < 3
    then return ()
    else do
      primFileClose n
      held <- readRef locks
      writeRef locks (filter (\(_, m, _) -> m /= n) held)

openFile :: FilePath -> IOMode -> IO Handle
openFile path mode = do
  held <- readRef locks
  let writing = mode /= ReadMode
      failed t said = ioError (IOError t "openFile" said Nothing (Just path))
  if any (\(p, _, w) -> p == path && (w || writing)) held
    then failed ResourceBusy "file is locked"
    else do
      mapM_ primPathChar path
      n <- primFileOpen (toInteger (fromEnum mode))
      if n >= 0
        then do
          writeRef locks ((path, n, writing) : held)
          r <- newRef (Opened 0 mode (BlockBuffering Nothing) (negate 1) False)
          return (Handle n path r)
        else
          if n == negate 1
            then failed NoSuchThing "No such file or directory"
            else
              if n == negate 2
                then failed PermissionDenied "Permission denied"
                else if n == negate 3 then failed ResourceBusy "file is locked" else failed OtherError ""

hClose :: Handle -> IO ()
hClose h@(Handle n _ r) =
  readRef r >>= \(Opened phase mode _ _ _) ->
    if phase == 2 then return () else (if mode == ReadMode then return () else primFileFlush n) >> shut h

-- The action is given the handle, which is closed however the action ends.
withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile path mode act = do
  h <- openFile path mode
  x <- catchIOError (act h) (\e -> hClose h >> ioError e)
  hClose h
  return x

hIsOpen :: Handle -> IO Bool
hIsOpen (Handle _ _ r) = readRef r >>= \(Opened phase _ _ _ _) -> return (phase == 0)

hIsClosed :: Handle -> IO Bool
hIsClosed (Handle _ _ r) = readRef r >>= \(Opened phase _ _ _ _) -> return (phase == 2)

hIsReadable :: Handle -> IO Bool
hIsReadable h = wantOpen "hIsReadable" h >>= \(Opened _ mode _ _ _) -> return (mode == ReadMode || mode == ReadWriteMode)

hIsWritable :: Handle -> IO Bool
hIsWritable h = wantOpen "hIsWritable" h >>= \(Opened _ mode _ _ _) -> return (mode /= ReadMode)

-- A file can be moved about in unless it is only being added to.
hIsSeekable :: Handle -> IO Bool
hIsSeekable h@(Handle n _ _) =
  wantOpen "hIsSeekable" h >>= \(Opened _ mode _ _ _) ->
    if n < 3 || mode == AppendMode then return False else primFileSeek n 1 0 >>= \at -> return (at >= 0)

hIsTerminalDevice :: Handle -> IO Bool
hIsTerminalDevice h@(Handle n _ _) = wantOpen "hIsTerminalDevice" h >> primIsTerminal n

-- Only a terminal echoes; what is asked of anything else is not kept.
hSetEcho :: Handle -> Bool -> IO ()
hSetEcho h@(Handle n _ r) echo =
  wantOpen "hSetEcho" h >>= \(Opened phase mode buffering held _) ->
    primIsTerminal n >>= \t -> if t then writeRef r (Opened phase mode buffering held echo) else return ()

hGetEcho :: Handle -> IO Bool
hGetEcho h@(Handle n _ _) = wantOpen "hGetEcho" h >>= \(Opened _ _ _ _ echo) -> primIsTerminal n >>= \t -> return (t && echo)

hShow :: Handle -> IO String
hShow h@(Handle _ _ r) =
  readRef r >>= \(Opened phase mode buffering _ _) ->
    return
      ( "{loc=" ++ show h ++ ",type="
          ++ (if phase == 2 then "closed" else if phase == 1 then "semi-closed" else show mode)
          ++ ",buffering="
          ++ show buffering
          ++ "}"
      )

-- How a handle is buffered is kept and given back; the machine decides
-- for itself what it holds, and hFlush sends that on.
hSetBuffering :: Handle -> BufferMode -> IO ()
hSetBuffering h@(Handle _ _ r) buffering =
  wantOpen "hSetBuffering" h >>= \(Opened phase mode _ held echo) -> case buffering of
    BlockBuffering (Just n) | n <= 0 -> refuse IllegalOperation "hSetBuffering" "illegal buffer size" h
    _ -> writeRef r (Opened phase mode buffering held echo)

hGetBuffering :: Handle -> IO BufferMode
hGetBuffering h = wantOpen "hGetBuffering" h >>= \(Opened _ _ buffering _ _) -> return buffering

hFlush :: Handle -> IO ()
hFlush h@(Handle n _ _) = wantWritable "hFlush" h >> primFileFlush n

hFileSize :: Handle -> IO Integer
hFileSize h@(Handle n _ _) =
  wantOpen "hFileSize" h >> primFileSeek n 3 0 >>= \size ->
    if size < 0 then refuse IllegalOperation "hFileSize" "not a regular file" h else return size

hSetFileSize :: Handle -> Integer -> IO ()
hSetFileSize h@(Handle n _ _) size =
  wantOpen "hSetFileSize" h >> primFileTruncate n size >>= \done ->
    if done < 0 then refuse IllegalOperation "hSetFileSize" "the file's size cannot be set" h else return ()

-- Where a handle is: where the machine is, less what was read ahead.
hTell :: Handle -> IO Integer
hTell h@(Handle n _ _) =
  wantOpen "hTell" h >>= \(Opened _ mode _ held _) ->
    (if mode == AppendMode then return (negate 1) else primFileSeek n 1 0) >>= \at ->
      if at < 0 then refuse IllegalOperation "hTell" "handle is not seekable" h else return (at - width held)

hSeek :: Handle -> SeekMode -> Integer -> IO ()
hSeek h@(Handle n _ r) whence offset =
  wantOpen "hSeek" h >>= \(Opened phase mode buffering held echo) -> do
    let from = if whence == RelativeSeek then offset - width held else offset
    at <- if mode == AppendMode then return (negate 1) else primFileSeek n (toInteger (fromEnum whence)) from
    if at < 0
      then refuse IllegalOperation "hSeek" "handle is not seekable" h
      else writeRef r (Opened phase mode buffering (negate 1) echo)

data HandlePosn = HandlePosn Handle Integer

instance Eq HandlePosn where
  HandlePosn h a == HandlePosn k b = h == k && a == b

instance Show HandlePosn where
  showsPrec p (HandlePosn h at) = showsPrec p h . showString " at position " . shows at

hGetPosn :: Handle -> IO HandlePosn
hGetPosn h = hTell h >>= \at -> return (HandlePosn h at)

hSetPosn :: HandlePosn -> IO ()
hSetPosn (HandlePosn h at) = hSeek h AbsoluteSeek at

hIsEOF :: Handle -> IO Bool
hIsEOF h@(Handle n _ _) = wantReadable "hIsEOF" h >>= \(Opened _ _ _ held _) -> if held < 0 then primHIsEOF n else return False

isEOF :: IO Bool
isEOF = hIsEOF stdin

-- Whether there is something to read, which here is waited for however
-- long was asked: at the end there never will be, and that is an error.
hWaitForInput :: Handle -> Int -> IO Bool
hWaitForInput h _ = hIsEOF h >>= \done -> if done then refuse EOF "hWaitForInput" "" h else return True

hReady :: Handle -> IO Bool
hReady h = hWaitForInput h 0

hLookAhead :: Handle -> IO Char
hLookAhead h@(Handle n _ r) =
  wantReadable "hLookAhead" h >>= \(Opened phase mode buffering held echo) ->
    if held >= 0
      then return (primIntToChar held)
      else
        raw n >>= \c ->
          if c < 0
            then refuse EOF "hLookAhead" "" h
            else writeRef r (Opened phase mode buffering c echo) >> return (primIntToChar c)

hGetChar :: Handle -> IO Char
hGetChar h = wantReadable "hGetChar" h >> next h >>= \c -> if c < 0 then refuse EOF "hGetChar" "" h else return (primIntToChar c)

-- A line, without its end. The last line need not have one; but where
-- there is nothing at all to read, that is the end of the input.
hGetLine :: Handle -> IO String
hGetLine h@(Handle n _ _) = wantReadable "hGetLine" h >> next h >>= \c -> if c < 0 then refuse EOF "hGetLine" "" h else go [] c
  where
    go acc c = if c < 0 || c == 10 then return (reverse acc) else raw n >>= go (primIntToChar c : acc)

-- Characters read one at a time, each when it is first looked at, and
-- something done when there are no more.
lazily :: IO Integer -> IO () -> String
lazily step done = primPerformIO (step >>= \c -> if c < 0 then done >> return [] else return (primIntToChar c : lazily step done))

-- All that is left to read, read as it is looked at. The handle is
-- semi-closed until the end is reached, and then closed; closing it
-- sooner ends what is read there.
hGetContents :: Handle -> IO String
hGetContents h@(Handle n _ r) =
  wantReadable "hGetContents" h >>= \(Opened _ mode buffering held echo) -> do
    writeRef r (Opened 1 mode buffering held echo)
    return (lazily step (shut h))
  where
    step =
      readRef r >>= \(Opened phase mode buffering held echo) ->
        if phase == 2
          then return (negate 1)
          else if held < 0 then raw n else writeRef r (Opened phase mode buffering (negate 1) echo) >> return held

hPutChar :: Handle -> Char -> IO ()
hPutChar h@(Handle n _ _) c = wantWritable "hPutChar" h >> primPutChar n c

hPutStr :: Handle -> String -> IO ()
hPutStr h@(Handle n _ _) s = wantWritable "hPutStr" h >> mapM_ (primPutChar n) s

hPutStrLn :: Handle -> String -> IO ()
hPutStrLn h s = hPutStr h s >> hPutChar h '\n'

hPrint :: Show a => Handle -> a -> IO ()
hPrint h x = hPutStrLn h (show x)

putChar :: Char -> IO ()
putChar = hPutChar stdout

putStr :: String -> IO ()
putStr = hPutStr stdout

putStrLn :: String -> IO ()
putStrLn s = putStr s >> putChar '\n'

print :: Show a => a -> IO ()
print x = putStrLn (show x)

getChar :: IO Char
getChar = hGetChar stdin

getLine :: IO String
getLine = hGetLine stdin

getContents :: IO String
getContents = hGetContents stdin

interact :: (String -> String) -> IO ()
interact f = getContents >>= \s -> putStr (f s)

exitWith :: Int -> IO a
exitWith n = leave (toInteger n)

-- The file is read as the string is looked at, and closed at its end.
readFile :: FilePath -> IO String
readFile path = openFile path ReadMode >>= hGetContents

writeFile :: FilePath -> String -> IO ()
writeFile path s = openFile path WriteMode >>= \h -> hPutStr h s >> hClose h

appendFile :: FilePath -> String -> IO ()
appendFile path s = openFile path AppendMode >>= \h -> hPutStr h s >> hClose h

-- Reading -----------------------------------------------------------------

type ReadS a = String -> [(a, String)]

class Read a where
  readsPrec :: Int -> ReadS a
  readList :: ReadS [a]
  readList = readParen False (\r -> [pr | ("[", s) <- lex r, pr <- first s])
    where
      first s = [([], t) | ("]", t) <- lex s] ++ [(x : xs, u) | (x, t) <- reads s, (xs, u) <- rest t]
      rest s = [([], t) | ("]", t) <- lex s] ++ [(x : xs, v) | (",", t) <- lex s, (x, u) <- reads t, (xs, v) <- rest u]

reads :: Read a => ReadS a
reads = readsPrec 0

read :: Read a => String -> a
read s = case [x | (x, t) <- reads s, ("", "") <- lex t] of
  [x] -> x
  [] -> error "Prelude.read: no parse"
  _ -> error "Prelude.read: ambiguous parse"

readParen :: Bool -> ReadS a -> ReadS a
readParen needed g = if needed then mandatory else optional
  where
    optional r = g r ++ mandatory r
    mandatory r = [(x, u) | ("(", s) <- lex r, (x, t) <- optional s, (")", u) <- lex t]

readIO :: Read a => String -> IO a
readIO s = case [x | (x, t) <- reads s, ("", "") <- lex t] of
  [x] -> return x
  [] -> ioError (userError "Prelude.readIO: no parse")
  _ -> ioError (userError "Prelude.readIO: ambiguous parse")

readLn :: Read a => IO a
readLn = getLine >>= readIO

-- One lexeme off the front of a string, after any space; or nothing
-- where the string does not start with one. As the Report defines it.
lex :: ReadS String
lex "" = [("", "")]
lex (c : s)
  | isSpace c = lex (dropWhile isSpace s)
  | c == '\'' = [('\'' : ch ++ "'", t) | (ch, '\'' : t) <- lexLitChar s, ch /= "'"]
  | c == '"' = [('"' : str, t) | (str, t) <- lexString s]
  | isSingle c = [([c], s)]
  | isSym c = [(c : sym, t) | (sym, t) <- [span isSym s]]
  | isAlpha c = [(c : nam, t) | (nam, t) <- [span isIdChar s]]
  | isDigit c = [(c : ds ++ fe, t) | (ds, s') <- [span isDigit s], (fe, t) <- lexFracExp s']
  | otherwise = []
  where
    isSingle x = x `elem` ",;()[]{}_`"
    isSym x = x `elem` "!@#$%&*+./<=>?\\^|:-~"
    isIdChar x = isAlphaNum x || x `elem` "_'"
    lexString ('"' : t) = [("\"", t)]
    lexString t = [(ch ++ str, v) | (ch, u) <- lexStrItem t, (str, v) <- lexString u]
    lexStrItem ('\\' : '&' : t) = [("\\&", t)]
    lexStrItem ('\\' : x : t) | isSpace x = [("\\&", u) | '\\' : u <- [dropWhile isSpace t]]
    lexStrItem t = lexLitChar t
    lexFracExp ('.' : d : ds) | isDigit d = [('.' : digits ++ e, u) | (digits, t) <- lexDigits (d : ds), (e, u) <- lexExp t]
    lexFracExp t = lexExp t
    lexExp (e : t)
      | e `elem` "eE" =
          [(e : sign : ds, v) | sign : u <- [t], sign `elem` "+-", (ds, v) <- lexDigits u]
            ++ [(e : ds, u) | (ds, u) <- lexDigits t]
    lexExp t = [("", t)]

lexDigits :: ReadS String
lexDigits = nonnull isDigit

nonnull :: (Char -> Bool) -> ReadS String
nonnull p s = [(cs, t) | (cs@(_ : _), t) <- [span p s]]

readSigned :: Real a => ReadS a -> ReadS a
readSigned readPos = readParen False signed
  where
    signed r = unsigned r ++ [(negate x, t) | ("-", s) <- lex r, (x, t) <- unsigned s]
    unsigned r = [(n, s) | (str, s) <- lex r, (n, "") <- readPos str]

-- A number in a base, given which characters are its digits and what
-- each is worth.
readInt :: Integral a => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a
readInt radix isDig digToInt s = [(foldl1 (\n d -> n * radix + d) (map (fromIntegral . digToInt) ds), r) | (ds, r) <- nonnull isDig s]

readDec, readOct, readHex :: Integral a => ReadS a
readDec = readInt 10 isDigit digitToInt
readOct = readInt 8 isOctDigit digitToInt
readHex = readInt 16 isHexDigit digitToInt

readFloat :: RealFrac a => ReadS a
readFloat r =
  [(fromRational ((n % 1) * 10 ^^ (k - d)), t) | (n, d, s) <- readFix r, (k, t) <- readExp s]
    ++ [(0 / 0, t) | ("NaN", t) <- lex r]
    ++ [(1 / 0, t) | ("Infinity", t) <- lex r]
  where
    readFix s = [(n, length ds', t) | (ds, u) <- lexDigits s, (ds', t) <- lexFrac u, (n, _) <- readDec (ds ++ ds')]
    lexFrac ('.' : ds) = lexDigits ds
    lexFrac s = [("", s)]
    readExp (e : s) | e `elem` "eE" = readExp' s
    readExp s = [(0, s)]
    readExp' ('-' : s) = [(negate k, t) | (k, t) <- readDec s]
    readExp' ('+' : s) = readDec s
    readExp' s = readDec s

instance Read Integer where
  readsPrec _ = readSigned readDec

instance Read Int where
  readsPrec p r = [(fromInteger n, s) | (n, s) <- readsPrec p r]

instance Read Double where
  readsPrec _ = readSigned readFloat

instance Read Float where
  readsPrec _ = readSigned readFloat

instance Read Char where
  readsPrec _ = readParen False (\r -> [(c, t) | ('\'' : s, t) <- lex r, (c, "'") <- readLitChar s])
  readList = readParen False (\r -> [(l, t) | ('"' : s, t) <- lex r, (l, _) <- readl s])
    where
      readl ('"' : _) = [("", "")]
      readl ('\\' : '&' : s) = readl s
      readl s = [(c : cs, u) | (c, t) <- readLitChar s, (cs, u) <- readl t]

instance Read a => Read [a] where
  readsPrec _ = readList

instance Read Bool where
  readsPrec _ = readParen False (\r -> [(True, t) | ("True", t) <- lex r] ++ [(False, t) | ("False", t) <- lex r])

instance Read a => Read (Maybe a) where
  readsPrec d r =
    readParen False (\s -> [(Nothing, t) | ("Nothing", t) <- lex s]) r
      ++ readParen (d > 10) (\s -> [(Just x, u) | ("Just", t) <- lex s, (x, u) <- readsPrec 11 t]) r

instance (Read a, Read b) => Read (Either a b) where
  readsPrec d r =
    readParen (d > 10) (\s -> [(Left x, u) | ("Left", t) <- lex s, (x, u) <- readsPrec 11 t]) r
      ++ readParen (d > 10) (\s -> [(Right x, u) | ("Right", t) <- lex s, (x, u) <- readsPrec 11 t]) r

instance Read () where
  readsPrec _ = readParen False (\r -> [((), t) | ("(", s) <- lex r, (")", t) <- lex s])

