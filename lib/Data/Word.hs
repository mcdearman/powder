-- Whole numbers of a fixed size, which wrap round. Each is an Integer
-- kept within its range.
module Data.Word (Word, Word8, Word16, Word32, Word64) where

import Data.Bits
import Data.Ix

newtype Word = MkWord Integer

-- The number in range that an integer is the same as, counting round.
toWord :: Integer -> Word
toWord x = MkWord (mod x 18446744073709551616)

instance Eq Word where
  MkWord a == MkWord b = a == b

instance Ord Word where
  compare (MkWord a) (MkWord b) = compare a b
  MkWord a <= MkWord b = a <= b
  MkWord a < MkWord b = a < b

instance Show Word where
  showsPrec p (MkWord a) = showsPrec p a

instance Read Word where
  readsPrec p s = [(toWord x, t) | (x, t) <- readsPrec p s]

instance Num Word where
  MkWord a + MkWord b = toWord (a + b)
  MkWord a - MkWord b = toWord (a - b)
  MkWord a * MkWord b = toWord (a * b)
  negate (MkWord a) = toWord (negate a)
  abs (MkWord a) = toWord (abs a)
  signum (MkWord a) = MkWord (signum a)
  fromInteger = toWord

instance Real Word where
  toRational (MkWord a) = toRational a

instance Enum Word where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toWord (toInteger n)
  fromEnum (MkWord a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkWord a) (MkWord b) = map MkWord (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkWord a) (MkWord b) (MkWord d) = map MkWord (enumFromThenTo a b d)

instance Integral Word where
  quot (MkWord a) (MkWord b) = toWord (quot a b)
  rem (MkWord a) (MkWord b) = toWord (rem a b)
  div (MkWord a) (MkWord b) = toWord (div a b)
  mod (MkWord a) (MkWord b) = toWord (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkWord a) = a

instance Bounded Word where
  minBound = MkWord (0)
  maxBound = MkWord 18446744073709551615

instance Ix Word where
  range (m, n) = [m .. n]
  index b@(m, _) i = if inRange b i then fromInteger (toInteger i - toInteger m) else error "Ix.index: index out of range"
  inRange (m, n) i = m <= i && i <= n

instance Bits Word where
  MkWord a .&. MkWord b = toWord (a .&. b)
  MkWord a .|. MkWord b = toWord (a .|. b)
  xor (MkWord a) (MkWord b) = toWord (xor a b)
  complement (MkWord a) = toWord (complement a)
  shiftL (MkWord a) n = toWord (shiftL a n)
  shiftR (MkWord a) n = toWord (shiftR a n)
  bit n = toWord (bit n)
  testBit (MkWord a) n = testBit a n
  rotate (MkWord a) n = let k = mod n 64; u = mod a 18446744073709551616 in toWord (u * 2 ^ k + div u (2 ^ (64 - k)))
  bitSize _ = 64
  isSigned _ = False
  popCount (MkWord a) = popCount (if a < 0 then a + 18446744073709551616 else a)

newtype Word8 = MkWord8 Integer

-- The number in range that an integer is the same as, counting round.
toWord8 :: Integer -> Word8
toWord8 x = MkWord8 (mod x 256)

instance Eq Word8 where
  MkWord8 a == MkWord8 b = a == b

instance Ord Word8 where
  compare (MkWord8 a) (MkWord8 b) = compare a b
  MkWord8 a <= MkWord8 b = a <= b
  MkWord8 a < MkWord8 b = a < b

instance Show Word8 where
  showsPrec p (MkWord8 a) = showsPrec p a

instance Read Word8 where
  readsPrec p s = [(toWord8 x, t) | (x, t) <- readsPrec p s]

instance Num Word8 where
  MkWord8 a + MkWord8 b = toWord8 (a + b)
  MkWord8 a - MkWord8 b = toWord8 (a - b)
  MkWord8 a * MkWord8 b = toWord8 (a * b)
  negate (MkWord8 a) = toWord8 (negate a)
  abs (MkWord8 a) = toWord8 (abs a)
  signum (MkWord8 a) = MkWord8 (signum a)
  fromInteger = toWord8

instance Real Word8 where
  toRational (MkWord8 a) = toRational a

instance Enum Word8 where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toWord8 (toInteger n)
  fromEnum (MkWord8 a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkWord8 a) (MkWord8 b) = map MkWord8 (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkWord8 a) (MkWord8 b) (MkWord8 d) = map MkWord8 (enumFromThenTo a b d)

instance Integral Word8 where
  quot (MkWord8 a) (MkWord8 b) = toWord8 (quot a b)
  rem (MkWord8 a) (MkWord8 b) = toWord8 (rem a b)
  div (MkWord8 a) (MkWord8 b) = toWord8 (div a b)
  mod (MkWord8 a) (MkWord8 b) = toWord8 (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkWord8 a) = a

instance Bounded Word8 where
  minBound = MkWord8 (0)
  maxBound = MkWord8 255

instance Ix Word8 where
  range (m, n) = [m .. n]
  index b@(m, _) i = if inRange b i then fromInteger (toInteger i - toInteger m) else error "Ix.index: index out of range"
  inRange (m, n) i = m <= i && i <= n

instance Bits Word8 where
  MkWord8 a .&. MkWord8 b = toWord8 (a .&. b)
  MkWord8 a .|. MkWord8 b = toWord8 (a .|. b)
  xor (MkWord8 a) (MkWord8 b) = toWord8 (xor a b)
  complement (MkWord8 a) = toWord8 (complement a)
  shiftL (MkWord8 a) n = toWord8 (shiftL a n)
  shiftR (MkWord8 a) n = toWord8 (shiftR a n)
  bit n = toWord8 (bit n)
  testBit (MkWord8 a) n = testBit a n
  rotate (MkWord8 a) n = let k = mod n 8; u = mod a 256 in toWord8 (u * 2 ^ k + div u (2 ^ (8 - k)))
  bitSize _ = 8
  isSigned _ = False
  popCount (MkWord8 a) = popCount (if a < 0 then a + 256 else a)

newtype Word16 = MkWord16 Integer

-- The number in range that an integer is the same as, counting round.
toWord16 :: Integer -> Word16
toWord16 x = MkWord16 (mod x 65536)

instance Eq Word16 where
  MkWord16 a == MkWord16 b = a == b

instance Ord Word16 where
  compare (MkWord16 a) (MkWord16 b) = compare a b
  MkWord16 a <= MkWord16 b = a <= b
  MkWord16 a < MkWord16 b = a < b

instance Show Word16 where
  showsPrec p (MkWord16 a) = showsPrec p a

instance Read Word16 where
  readsPrec p s = [(toWord16 x, t) | (x, t) <- readsPrec p s]

instance Num Word16 where
  MkWord16 a + MkWord16 b = toWord16 (a + b)
  MkWord16 a - MkWord16 b = toWord16 (a - b)
  MkWord16 a * MkWord16 b = toWord16 (a * b)
  negate (MkWord16 a) = toWord16 (negate a)
  abs (MkWord16 a) = toWord16 (abs a)
  signum (MkWord16 a) = MkWord16 (signum a)
  fromInteger = toWord16

instance Real Word16 where
  toRational (MkWord16 a) = toRational a

instance Enum Word16 where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toWord16 (toInteger n)
  fromEnum (MkWord16 a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkWord16 a) (MkWord16 b) = map MkWord16 (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkWord16 a) (MkWord16 b) (MkWord16 d) = map MkWord16 (enumFromThenTo a b d)

instance Integral Word16 where
  quot (MkWord16 a) (MkWord16 b) = toWord16 (quot a b)
  rem (MkWord16 a) (MkWord16 b) = toWord16 (rem a b)
  div (MkWord16 a) (MkWord16 b) = toWord16 (div a b)
  mod (MkWord16 a) (MkWord16 b) = toWord16 (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkWord16 a) = a

instance Bounded Word16 where
  minBound = MkWord16 (0)
  maxBound = MkWord16 65535

instance Ix Word16 where
  range (m, n) = [m .. n]
  index b@(m, _) i = if inRange b i then fromInteger (toInteger i - toInteger m) else error "Ix.index: index out of range"
  inRange (m, n) i = m <= i && i <= n

instance Bits Word16 where
  MkWord16 a .&. MkWord16 b = toWord16 (a .&. b)
  MkWord16 a .|. MkWord16 b = toWord16 (a .|. b)
  xor (MkWord16 a) (MkWord16 b) = toWord16 (xor a b)
  complement (MkWord16 a) = toWord16 (complement a)
  shiftL (MkWord16 a) n = toWord16 (shiftL a n)
  shiftR (MkWord16 a) n = toWord16 (shiftR a n)
  bit n = toWord16 (bit n)
  testBit (MkWord16 a) n = testBit a n
  rotate (MkWord16 a) n = let k = mod n 16; u = mod a 65536 in toWord16 (u * 2 ^ k + div u (2 ^ (16 - k)))
  bitSize _ = 16
  isSigned _ = False
  popCount (MkWord16 a) = popCount (if a < 0 then a + 65536 else a)

newtype Word32 = MkWord32 Integer

-- The number in range that an integer is the same as, counting round.
toWord32 :: Integer -> Word32
toWord32 x = MkWord32 (mod x 4294967296)

instance Eq Word32 where
  MkWord32 a == MkWord32 b = a == b

instance Ord Word32 where
  compare (MkWord32 a) (MkWord32 b) = compare a b
  MkWord32 a <= MkWord32 b = a <= b
  MkWord32 a < MkWord32 b = a < b

instance Show Word32 where
  showsPrec p (MkWord32 a) = showsPrec p a

instance Read Word32 where
  readsPrec p s = [(toWord32 x, t) | (x, t) <- readsPrec p s]

instance Num Word32 where
  MkWord32 a + MkWord32 b = toWord32 (a + b)
  MkWord32 a - MkWord32 b = toWord32 (a - b)
  MkWord32 a * MkWord32 b = toWord32 (a * b)
  negate (MkWord32 a) = toWord32 (negate a)
  abs (MkWord32 a) = toWord32 (abs a)
  signum (MkWord32 a) = MkWord32 (signum a)
  fromInteger = toWord32

instance Real Word32 where
  toRational (MkWord32 a) = toRational a

instance Enum Word32 where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toWord32 (toInteger n)
  fromEnum (MkWord32 a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkWord32 a) (MkWord32 b) = map MkWord32 (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkWord32 a) (MkWord32 b) (MkWord32 d) = map MkWord32 (enumFromThenTo a b d)

instance Integral Word32 where
  quot (MkWord32 a) (MkWord32 b) = toWord32 (quot a b)
  rem (MkWord32 a) (MkWord32 b) = toWord32 (rem a b)
  div (MkWord32 a) (MkWord32 b) = toWord32 (div a b)
  mod (MkWord32 a) (MkWord32 b) = toWord32 (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkWord32 a) = a

instance Bounded Word32 where
  minBound = MkWord32 (0)
  maxBound = MkWord32 4294967295

instance Ix Word32 where
  range (m, n) = [m .. n]
  index b@(m, _) i = if inRange b i then fromInteger (toInteger i - toInteger m) else error "Ix.index: index out of range"
  inRange (m, n) i = m <= i && i <= n

instance Bits Word32 where
  MkWord32 a .&. MkWord32 b = toWord32 (a .&. b)
  MkWord32 a .|. MkWord32 b = toWord32 (a .|. b)
  xor (MkWord32 a) (MkWord32 b) = toWord32 (xor a b)
  complement (MkWord32 a) = toWord32 (complement a)
  shiftL (MkWord32 a) n = toWord32 (shiftL a n)
  shiftR (MkWord32 a) n = toWord32 (shiftR a n)
  bit n = toWord32 (bit n)
  testBit (MkWord32 a) n = testBit a n
  rotate (MkWord32 a) n = let k = mod n 32; u = mod a 4294967296 in toWord32 (u * 2 ^ k + div u (2 ^ (32 - k)))
  bitSize _ = 32
  isSigned _ = False
  popCount (MkWord32 a) = popCount (if a < 0 then a + 4294967296 else a)

newtype Word64 = MkWord64 Integer

-- The number in range that an integer is the same as, counting round.
toWord64 :: Integer -> Word64
toWord64 x = MkWord64 (mod x 18446744073709551616)

instance Eq Word64 where
  MkWord64 a == MkWord64 b = a == b

instance Ord Word64 where
  compare (MkWord64 a) (MkWord64 b) = compare a b
  MkWord64 a <= MkWord64 b = a <= b
  MkWord64 a < MkWord64 b = a < b

instance Show Word64 where
  showsPrec p (MkWord64 a) = showsPrec p a

instance Read Word64 where
  readsPrec p s = [(toWord64 x, t) | (x, t) <- readsPrec p s]

instance Num Word64 where
  MkWord64 a + MkWord64 b = toWord64 (a + b)
  MkWord64 a - MkWord64 b = toWord64 (a - b)
  MkWord64 a * MkWord64 b = toWord64 (a * b)
  negate (MkWord64 a) = toWord64 (negate a)
  abs (MkWord64 a) = toWord64 (abs a)
  signum (MkWord64 a) = MkWord64 (signum a)
  fromInteger = toWord64

instance Real Word64 where
  toRational (MkWord64 a) = toRational a

instance Enum Word64 where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toWord64 (toInteger n)
  fromEnum (MkWord64 a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkWord64 a) (MkWord64 b) = map MkWord64 (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkWord64 a) (MkWord64 b) (MkWord64 d) = map MkWord64 (enumFromThenTo a b d)

instance Integral Word64 where
  quot (MkWord64 a) (MkWord64 b) = toWord64 (quot a b)
  rem (MkWord64 a) (MkWord64 b) = toWord64 (rem a b)
  div (MkWord64 a) (MkWord64 b) = toWord64 (div a b)
  mod (MkWord64 a) (MkWord64 b) = toWord64 (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkWord64 a) = a

instance Bounded Word64 where
  minBound = MkWord64 (0)
  maxBound = MkWord64 18446744073709551615

instance Ix Word64 where
  range (m, n) = [m .. n]
  index b@(m, _) i = if inRange b i then fromInteger (toInteger i - toInteger m) else error "Ix.index: index out of range"
  inRange (m, n) i = m <= i && i <= n

instance Bits Word64 where
  MkWord64 a .&. MkWord64 b = toWord64 (a .&. b)
  MkWord64 a .|. MkWord64 b = toWord64 (a .|. b)
  xor (MkWord64 a) (MkWord64 b) = toWord64 (xor a b)
  complement (MkWord64 a) = toWord64 (complement a)
  shiftL (MkWord64 a) n = toWord64 (shiftL a n)
  shiftR (MkWord64 a) n = toWord64 (shiftR a n)
  bit n = toWord64 (bit n)
  testBit (MkWord64 a) n = testBit a n
  rotate (MkWord64 a) n = let k = mod n 64; u = mod a 18446744073709551616 in toWord64 (u * 2 ^ k + div u (2 ^ (64 - k)))
  bitSize _ = 64
  isSigned _ = False
  popCount (MkWord64 a) = popCount (if a < 0 then a + 18446744073709551616 else a)
