-- The types of C's numbers. Each whole number is an Integer kept within
-- its size, a CDouble is a Double and a CFloat a Float.
module Foreign.C.Types (CChar, CSChar, CUChar, CShort, CUShort, CInt, CUInt, CLong, CULong, CLLong, CULLong, CSize, CDouble, CFloat, CPtrdiff, CWchar, CSigAtomic, CClock, CTime, CIntPtr, CUIntPtr, CIntMax, CUIntMax, CFile, CFpos, CJmpBuf) where

import Data.Bits

newtype CChar = MkCChar Integer

toCChar :: Integer -> CChar
toCChar x = MkCChar (mod (x + 128) 256 - 128)

instance Eq CChar where
  MkCChar a == MkCChar b = a == b

instance Ord CChar where
  compare (MkCChar a) (MkCChar b) = compare a b
  MkCChar a <= MkCChar b = a <= b
  MkCChar a < MkCChar b = a < b

instance Show CChar where
  showsPrec p (MkCChar a) = showsPrec p a

instance Read CChar where
  readsPrec p s = [(toCChar x, t) | (x, t) <- readsPrec p s]

instance Num CChar where
  MkCChar a + MkCChar b = toCChar (a + b)
  MkCChar a - MkCChar b = toCChar (a - b)
  MkCChar a * MkCChar b = toCChar (a * b)
  negate (MkCChar a) = toCChar (negate a)
  abs (MkCChar a) = toCChar (abs a)
  signum (MkCChar a) = MkCChar (signum a)
  fromInteger = toCChar

instance Real CChar where
  toRational (MkCChar a) = toRational a

instance Enum CChar where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toCChar (toInteger n)
  fromEnum (MkCChar a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkCChar a) (MkCChar b) = map MkCChar (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkCChar a) (MkCChar b) (MkCChar d) = map MkCChar (enumFromThenTo a b d)

instance Integral CChar where
  quot (MkCChar a) (MkCChar b) = toCChar (quot a b)
  rem (MkCChar a) (MkCChar b) = toCChar (rem a b)
  div (MkCChar a) (MkCChar b) = toCChar (div a b)
  mod (MkCChar a) (MkCChar b) = toCChar (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkCChar a) = a

instance Bounded CChar where
  minBound = MkCChar (negate 128)
  maxBound = MkCChar 127

newtype CSChar = MkCSChar Integer

toCSChar :: Integer -> CSChar
toCSChar x = MkCSChar (mod (x + 128) 256 - 128)

instance Eq CSChar where
  MkCSChar a == MkCSChar b = a == b

instance Ord CSChar where
  compare (MkCSChar a) (MkCSChar b) = compare a b
  MkCSChar a <= MkCSChar b = a <= b
  MkCSChar a < MkCSChar b = a < b

instance Show CSChar where
  showsPrec p (MkCSChar a) = showsPrec p a

instance Read CSChar where
  readsPrec p s = [(toCSChar x, t) | (x, t) <- readsPrec p s]

instance Num CSChar where
  MkCSChar a + MkCSChar b = toCSChar (a + b)
  MkCSChar a - MkCSChar b = toCSChar (a - b)
  MkCSChar a * MkCSChar b = toCSChar (a * b)
  negate (MkCSChar a) = toCSChar (negate a)
  abs (MkCSChar a) = toCSChar (abs a)
  signum (MkCSChar a) = MkCSChar (signum a)
  fromInteger = toCSChar

instance Real CSChar where
  toRational (MkCSChar a) = toRational a

instance Enum CSChar where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toCSChar (toInteger n)
  fromEnum (MkCSChar a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkCSChar a) (MkCSChar b) = map MkCSChar (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkCSChar a) (MkCSChar b) (MkCSChar d) = map MkCSChar (enumFromThenTo a b d)

instance Integral CSChar where
  quot (MkCSChar a) (MkCSChar b) = toCSChar (quot a b)
  rem (MkCSChar a) (MkCSChar b) = toCSChar (rem a b)
  div (MkCSChar a) (MkCSChar b) = toCSChar (div a b)
  mod (MkCSChar a) (MkCSChar b) = toCSChar (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkCSChar a) = a

instance Bounded CSChar where
  minBound = MkCSChar (negate 128)
  maxBound = MkCSChar 127

newtype CUChar = MkCUChar Integer

toCUChar :: Integer -> CUChar
toCUChar x = MkCUChar (mod x 256)

instance Eq CUChar where
  MkCUChar a == MkCUChar b = a == b

instance Ord CUChar where
  compare (MkCUChar a) (MkCUChar b) = compare a b
  MkCUChar a <= MkCUChar b = a <= b
  MkCUChar a < MkCUChar b = a < b

instance Show CUChar where
  showsPrec p (MkCUChar a) = showsPrec p a

instance Read CUChar where
  readsPrec p s = [(toCUChar x, t) | (x, t) <- readsPrec p s]

instance Num CUChar where
  MkCUChar a + MkCUChar b = toCUChar (a + b)
  MkCUChar a - MkCUChar b = toCUChar (a - b)
  MkCUChar a * MkCUChar b = toCUChar (a * b)
  negate (MkCUChar a) = toCUChar (negate a)
  abs (MkCUChar a) = toCUChar (abs a)
  signum (MkCUChar a) = MkCUChar (signum a)
  fromInteger = toCUChar

instance Real CUChar where
  toRational (MkCUChar a) = toRational a

instance Enum CUChar where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toCUChar (toInteger n)
  fromEnum (MkCUChar a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkCUChar a) (MkCUChar b) = map MkCUChar (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkCUChar a) (MkCUChar b) (MkCUChar d) = map MkCUChar (enumFromThenTo a b d)

instance Integral CUChar where
  quot (MkCUChar a) (MkCUChar b) = toCUChar (quot a b)
  rem (MkCUChar a) (MkCUChar b) = toCUChar (rem a b)
  div (MkCUChar a) (MkCUChar b) = toCUChar (div a b)
  mod (MkCUChar a) (MkCUChar b) = toCUChar (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkCUChar a) = a

instance Bounded CUChar where
  minBound = MkCUChar (0)
  maxBound = MkCUChar 255

newtype CShort = MkCShort Integer

toCShort :: Integer -> CShort
toCShort x = MkCShort (mod (x + 32768) 65536 - 32768)

instance Eq CShort where
  MkCShort a == MkCShort b = a == b

instance Ord CShort where
  compare (MkCShort a) (MkCShort b) = compare a b
  MkCShort a <= MkCShort b = a <= b
  MkCShort a < MkCShort b = a < b

instance Show CShort where
  showsPrec p (MkCShort a) = showsPrec p a

instance Read CShort where
  readsPrec p s = [(toCShort x, t) | (x, t) <- readsPrec p s]

instance Num CShort where
  MkCShort a + MkCShort b = toCShort (a + b)
  MkCShort a - MkCShort b = toCShort (a - b)
  MkCShort a * MkCShort b = toCShort (a * b)
  negate (MkCShort a) = toCShort (negate a)
  abs (MkCShort a) = toCShort (abs a)
  signum (MkCShort a) = MkCShort (signum a)
  fromInteger = toCShort

instance Real CShort where
  toRational (MkCShort a) = toRational a

instance Enum CShort where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toCShort (toInteger n)
  fromEnum (MkCShort a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkCShort a) (MkCShort b) = map MkCShort (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkCShort a) (MkCShort b) (MkCShort d) = map MkCShort (enumFromThenTo a b d)

instance Integral CShort where
  quot (MkCShort a) (MkCShort b) = toCShort (quot a b)
  rem (MkCShort a) (MkCShort b) = toCShort (rem a b)
  div (MkCShort a) (MkCShort b) = toCShort (div a b)
  mod (MkCShort a) (MkCShort b) = toCShort (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkCShort a) = a

instance Bounded CShort where
  minBound = MkCShort (negate 32768)
  maxBound = MkCShort 32767

newtype CUShort = MkCUShort Integer

toCUShort :: Integer -> CUShort
toCUShort x = MkCUShort (mod x 65536)

instance Eq CUShort where
  MkCUShort a == MkCUShort b = a == b

instance Ord CUShort where
  compare (MkCUShort a) (MkCUShort b) = compare a b
  MkCUShort a <= MkCUShort b = a <= b
  MkCUShort a < MkCUShort b = a < b

instance Show CUShort where
  showsPrec p (MkCUShort a) = showsPrec p a

instance Read CUShort where
  readsPrec p s = [(toCUShort x, t) | (x, t) <- readsPrec p s]

instance Num CUShort where
  MkCUShort a + MkCUShort b = toCUShort (a + b)
  MkCUShort a - MkCUShort b = toCUShort (a - b)
  MkCUShort a * MkCUShort b = toCUShort (a * b)
  negate (MkCUShort a) = toCUShort (negate a)
  abs (MkCUShort a) = toCUShort (abs a)
  signum (MkCUShort a) = MkCUShort (signum a)
  fromInteger = toCUShort

instance Real CUShort where
  toRational (MkCUShort a) = toRational a

instance Enum CUShort where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toCUShort (toInteger n)
  fromEnum (MkCUShort a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkCUShort a) (MkCUShort b) = map MkCUShort (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkCUShort a) (MkCUShort b) (MkCUShort d) = map MkCUShort (enumFromThenTo a b d)

instance Integral CUShort where
  quot (MkCUShort a) (MkCUShort b) = toCUShort (quot a b)
  rem (MkCUShort a) (MkCUShort b) = toCUShort (rem a b)
  div (MkCUShort a) (MkCUShort b) = toCUShort (div a b)
  mod (MkCUShort a) (MkCUShort b) = toCUShort (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkCUShort a) = a

instance Bounded CUShort where
  minBound = MkCUShort (0)
  maxBound = MkCUShort 65535

newtype CInt = MkCInt Integer

toCInt :: Integer -> CInt
toCInt x = MkCInt (mod (x + 2147483648) 4294967296 - 2147483648)

instance Eq CInt where
  MkCInt a == MkCInt b = a == b

instance Ord CInt where
  compare (MkCInt a) (MkCInt b) = compare a b
  MkCInt a <= MkCInt b = a <= b
  MkCInt a < MkCInt b = a < b

instance Show CInt where
  showsPrec p (MkCInt a) = showsPrec p a

instance Read CInt where
  readsPrec p s = [(toCInt x, t) | (x, t) <- readsPrec p s]

instance Num CInt where
  MkCInt a + MkCInt b = toCInt (a + b)
  MkCInt a - MkCInt b = toCInt (a - b)
  MkCInt a * MkCInt b = toCInt (a * b)
  negate (MkCInt a) = toCInt (negate a)
  abs (MkCInt a) = toCInt (abs a)
  signum (MkCInt a) = MkCInt (signum a)
  fromInteger = toCInt

instance Real CInt where
  toRational (MkCInt a) = toRational a

instance Enum CInt where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toCInt (toInteger n)
  fromEnum (MkCInt a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkCInt a) (MkCInt b) = map MkCInt (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkCInt a) (MkCInt b) (MkCInt d) = map MkCInt (enumFromThenTo a b d)

instance Integral CInt where
  quot (MkCInt a) (MkCInt b) = toCInt (quot a b)
  rem (MkCInt a) (MkCInt b) = toCInt (rem a b)
  div (MkCInt a) (MkCInt b) = toCInt (div a b)
  mod (MkCInt a) (MkCInt b) = toCInt (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkCInt a) = a

instance Bounded CInt where
  minBound = MkCInt (negate 2147483648)
  maxBound = MkCInt 2147483647

newtype CUInt = MkCUInt Integer

toCUInt :: Integer -> CUInt
toCUInt x = MkCUInt (mod x 4294967296)

instance Eq CUInt where
  MkCUInt a == MkCUInt b = a == b

instance Ord CUInt where
  compare (MkCUInt a) (MkCUInt b) = compare a b
  MkCUInt a <= MkCUInt b = a <= b
  MkCUInt a < MkCUInt b = a < b

instance Show CUInt where
  showsPrec p (MkCUInt a) = showsPrec p a

instance Read CUInt where
  readsPrec p s = [(toCUInt x, t) | (x, t) <- readsPrec p s]

instance Num CUInt where
  MkCUInt a + MkCUInt b = toCUInt (a + b)
  MkCUInt a - MkCUInt b = toCUInt (a - b)
  MkCUInt a * MkCUInt b = toCUInt (a * b)
  negate (MkCUInt a) = toCUInt (negate a)
  abs (MkCUInt a) = toCUInt (abs a)
  signum (MkCUInt a) = MkCUInt (signum a)
  fromInteger = toCUInt

instance Real CUInt where
  toRational (MkCUInt a) = toRational a

instance Enum CUInt where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toCUInt (toInteger n)
  fromEnum (MkCUInt a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkCUInt a) (MkCUInt b) = map MkCUInt (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkCUInt a) (MkCUInt b) (MkCUInt d) = map MkCUInt (enumFromThenTo a b d)

instance Integral CUInt where
  quot (MkCUInt a) (MkCUInt b) = toCUInt (quot a b)
  rem (MkCUInt a) (MkCUInt b) = toCUInt (rem a b)
  div (MkCUInt a) (MkCUInt b) = toCUInt (div a b)
  mod (MkCUInt a) (MkCUInt b) = toCUInt (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkCUInt a) = a

instance Bounded CUInt where
  minBound = MkCUInt (0)
  maxBound = MkCUInt 4294967295

newtype CLong = MkCLong Integer

toCLong :: Integer -> CLong
toCLong x = MkCLong (mod (x + 9223372036854775808) 18446744073709551616 - 9223372036854775808)

instance Eq CLong where
  MkCLong a == MkCLong b = a == b

instance Ord CLong where
  compare (MkCLong a) (MkCLong b) = compare a b
  MkCLong a <= MkCLong b = a <= b
  MkCLong a < MkCLong b = a < b

instance Show CLong where
  showsPrec p (MkCLong a) = showsPrec p a

instance Read CLong where
  readsPrec p s = [(toCLong x, t) | (x, t) <- readsPrec p s]

instance Num CLong where
  MkCLong a + MkCLong b = toCLong (a + b)
  MkCLong a - MkCLong b = toCLong (a - b)
  MkCLong a * MkCLong b = toCLong (a * b)
  negate (MkCLong a) = toCLong (negate a)
  abs (MkCLong a) = toCLong (abs a)
  signum (MkCLong a) = MkCLong (signum a)
  fromInteger = toCLong

instance Real CLong where
  toRational (MkCLong a) = toRational a

instance Enum CLong where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toCLong (toInteger n)
  fromEnum (MkCLong a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkCLong a) (MkCLong b) = map MkCLong (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkCLong a) (MkCLong b) (MkCLong d) = map MkCLong (enumFromThenTo a b d)

instance Integral CLong where
  quot (MkCLong a) (MkCLong b) = toCLong (quot a b)
  rem (MkCLong a) (MkCLong b) = toCLong (rem a b)
  div (MkCLong a) (MkCLong b) = toCLong (div a b)
  mod (MkCLong a) (MkCLong b) = toCLong (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkCLong a) = a

instance Bounded CLong where
  minBound = MkCLong (negate 9223372036854775808)
  maxBound = MkCLong 9223372036854775807

newtype CULong = MkCULong Integer

toCULong :: Integer -> CULong
toCULong x = MkCULong (mod x 18446744073709551616)

instance Eq CULong where
  MkCULong a == MkCULong b = a == b

instance Ord CULong where
  compare (MkCULong a) (MkCULong b) = compare a b
  MkCULong a <= MkCULong b = a <= b
  MkCULong a < MkCULong b = a < b

instance Show CULong where
  showsPrec p (MkCULong a) = showsPrec p a

instance Read CULong where
  readsPrec p s = [(toCULong x, t) | (x, t) <- readsPrec p s]

instance Num CULong where
  MkCULong a + MkCULong b = toCULong (a + b)
  MkCULong a - MkCULong b = toCULong (a - b)
  MkCULong a * MkCULong b = toCULong (a * b)
  negate (MkCULong a) = toCULong (negate a)
  abs (MkCULong a) = toCULong (abs a)
  signum (MkCULong a) = MkCULong (signum a)
  fromInteger = toCULong

instance Real CULong where
  toRational (MkCULong a) = toRational a

instance Enum CULong where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toCULong (toInteger n)
  fromEnum (MkCULong a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkCULong a) (MkCULong b) = map MkCULong (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkCULong a) (MkCULong b) (MkCULong d) = map MkCULong (enumFromThenTo a b d)

instance Integral CULong where
  quot (MkCULong a) (MkCULong b) = toCULong (quot a b)
  rem (MkCULong a) (MkCULong b) = toCULong (rem a b)
  div (MkCULong a) (MkCULong b) = toCULong (div a b)
  mod (MkCULong a) (MkCULong b) = toCULong (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkCULong a) = a

instance Bounded CULong where
  minBound = MkCULong (0)
  maxBound = MkCULong 18446744073709551615

newtype CLLong = MkCLLong Integer

toCLLong :: Integer -> CLLong
toCLLong x = MkCLLong (mod (x + 9223372036854775808) 18446744073709551616 - 9223372036854775808)

instance Eq CLLong where
  MkCLLong a == MkCLLong b = a == b

instance Ord CLLong where
  compare (MkCLLong a) (MkCLLong b) = compare a b
  MkCLLong a <= MkCLLong b = a <= b
  MkCLLong a < MkCLLong b = a < b

instance Show CLLong where
  showsPrec p (MkCLLong a) = showsPrec p a

instance Read CLLong where
  readsPrec p s = [(toCLLong x, t) | (x, t) <- readsPrec p s]

instance Num CLLong where
  MkCLLong a + MkCLLong b = toCLLong (a + b)
  MkCLLong a - MkCLLong b = toCLLong (a - b)
  MkCLLong a * MkCLLong b = toCLLong (a * b)
  negate (MkCLLong a) = toCLLong (negate a)
  abs (MkCLLong a) = toCLLong (abs a)
  signum (MkCLLong a) = MkCLLong (signum a)
  fromInteger = toCLLong

instance Real CLLong where
  toRational (MkCLLong a) = toRational a

instance Enum CLLong where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toCLLong (toInteger n)
  fromEnum (MkCLLong a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkCLLong a) (MkCLLong b) = map MkCLLong (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkCLLong a) (MkCLLong b) (MkCLLong d) = map MkCLLong (enumFromThenTo a b d)

instance Integral CLLong where
  quot (MkCLLong a) (MkCLLong b) = toCLLong (quot a b)
  rem (MkCLLong a) (MkCLLong b) = toCLLong (rem a b)
  div (MkCLLong a) (MkCLLong b) = toCLLong (div a b)
  mod (MkCLLong a) (MkCLLong b) = toCLLong (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkCLLong a) = a

instance Bounded CLLong where
  minBound = MkCLLong (negate 9223372036854775808)
  maxBound = MkCLLong 9223372036854775807

newtype CULLong = MkCULLong Integer

toCULLong :: Integer -> CULLong
toCULLong x = MkCULLong (mod x 18446744073709551616)

instance Eq CULLong where
  MkCULLong a == MkCULLong b = a == b

instance Ord CULLong where
  compare (MkCULLong a) (MkCULLong b) = compare a b
  MkCULLong a <= MkCULLong b = a <= b
  MkCULLong a < MkCULLong b = a < b

instance Show CULLong where
  showsPrec p (MkCULLong a) = showsPrec p a

instance Read CULLong where
  readsPrec p s = [(toCULLong x, t) | (x, t) <- readsPrec p s]

instance Num CULLong where
  MkCULLong a + MkCULLong b = toCULLong (a + b)
  MkCULLong a - MkCULLong b = toCULLong (a - b)
  MkCULLong a * MkCULLong b = toCULLong (a * b)
  negate (MkCULLong a) = toCULLong (negate a)
  abs (MkCULLong a) = toCULLong (abs a)
  signum (MkCULLong a) = MkCULLong (signum a)
  fromInteger = toCULLong

instance Real CULLong where
  toRational (MkCULLong a) = toRational a

instance Enum CULLong where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toCULLong (toInteger n)
  fromEnum (MkCULLong a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkCULLong a) (MkCULLong b) = map MkCULLong (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkCULLong a) (MkCULLong b) (MkCULLong d) = map MkCULLong (enumFromThenTo a b d)

instance Integral CULLong where
  quot (MkCULLong a) (MkCULLong b) = toCULLong (quot a b)
  rem (MkCULLong a) (MkCULLong b) = toCULLong (rem a b)
  div (MkCULLong a) (MkCULLong b) = toCULLong (div a b)
  mod (MkCULLong a) (MkCULLong b) = toCULLong (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkCULLong a) = a

instance Bounded CULLong where
  minBound = MkCULLong (0)
  maxBound = MkCULLong 18446744073709551615

newtype CSize = MkCSize Integer

toCSize :: Integer -> CSize
toCSize x = MkCSize (mod x 18446744073709551616)

instance Eq CSize where
  MkCSize a == MkCSize b = a == b

instance Ord CSize where
  compare (MkCSize a) (MkCSize b) = compare a b
  MkCSize a <= MkCSize b = a <= b
  MkCSize a < MkCSize b = a < b

instance Show CSize where
  showsPrec p (MkCSize a) = showsPrec p a

instance Read CSize where
  readsPrec p s = [(toCSize x, t) | (x, t) <- readsPrec p s]

instance Num CSize where
  MkCSize a + MkCSize b = toCSize (a + b)
  MkCSize a - MkCSize b = toCSize (a - b)
  MkCSize a * MkCSize b = toCSize (a * b)
  negate (MkCSize a) = toCSize (negate a)
  abs (MkCSize a) = toCSize (abs a)
  signum (MkCSize a) = MkCSize (signum a)
  fromInteger = toCSize

instance Real CSize where
  toRational (MkCSize a) = toRational a

instance Enum CSize where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toCSize (toInteger n)
  fromEnum (MkCSize a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkCSize a) (MkCSize b) = map MkCSize (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkCSize a) (MkCSize b) (MkCSize d) = map MkCSize (enumFromThenTo a b d)

instance Integral CSize where
  quot (MkCSize a) (MkCSize b) = toCSize (quot a b)
  rem (MkCSize a) (MkCSize b) = toCSize (rem a b)
  div (MkCSize a) (MkCSize b) = toCSize (div a b)
  mod (MkCSize a) (MkCSize b) = toCSize (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkCSize a) = a

instance Bounded CSize where
  minBound = MkCSize (0)
  maxBound = MkCSize 18446744073709551615

newtype CDouble = MkCDouble Double

instance Eq CDouble where
  MkCDouble a == MkCDouble b = a == b

instance Ord CDouble where
  MkCDouble a <= MkCDouble b = a <= b
  MkCDouble a < MkCDouble b = a < b

instance Show CDouble where
  showsPrec p (MkCDouble a) = showsPrec p a

instance Read CDouble where
  readsPrec p s = [(MkCDouble x, t) | (x, t) <- readsPrec p s]

instance Num CDouble where
  MkCDouble a + MkCDouble b = MkCDouble (a + b)
  MkCDouble a - MkCDouble b = MkCDouble (a - b)
  MkCDouble a * MkCDouble b = MkCDouble (a * b)
  negate (MkCDouble a) = MkCDouble (negate a)
  abs (MkCDouble a) = MkCDouble (abs a)
  signum (MkCDouble a) = MkCDouble (signum a)
  fromInteger n = MkCDouble (fromInteger n)

instance Real CDouble where
  toRational (MkCDouble a) = toRational a

instance Fractional CDouble where
  MkCDouble a / MkCDouble b = MkCDouble (a / b)
  fromRational q = MkCDouble (fromRational q)

instance Floating CDouble where
  pi = MkCDouble pi
  exp (MkCDouble a) = MkCDouble (exp a)
  log (MkCDouble a) = MkCDouble (log a)
  sqrt (MkCDouble a) = MkCDouble (sqrt a)
  sin (MkCDouble a) = MkCDouble (sin a)
  cos (MkCDouble a) = MkCDouble (cos a)
  asin (MkCDouble a) = MkCDouble (asin a)
  acos (MkCDouble a) = MkCDouble (acos a)
  atan (MkCDouble a) = MkCDouble (atan a)
  sinh (MkCDouble a) = MkCDouble (sinh a)
  cosh (MkCDouble a) = MkCDouble (cosh a)
  asinh (MkCDouble a) = MkCDouble (asinh a)
  acosh (MkCDouble a) = MkCDouble (acosh a)
  atanh (MkCDouble a) = MkCDouble (atanh a)
  MkCDouble a ** MkCDouble b = MkCDouble (a ** b)

instance RealFrac CDouble where
  properFraction (MkCDouble a) = let (n, r) = properFraction a in (n, MkCDouble r)

instance Enum CDouble where
  succ x = x + 1
  pred x = x - 1
  toEnum n = MkCDouble (toEnum n)
  fromEnum (MkCDouble a) = fromEnum a
  enumFrom (MkCDouble a) = map MkCDouble (enumFrom a)
  enumFromThen (MkCDouble a) (MkCDouble b) = map MkCDouble (enumFromThen a b)
  enumFromTo (MkCDouble a) (MkCDouble b) = map MkCDouble (enumFromTo a b)
  enumFromThenTo (MkCDouble a) (MkCDouble b) (MkCDouble c) = map MkCDouble (enumFromThenTo a b c)

newtype CFloat = MkCFloat Float

instance Eq CFloat where
  MkCFloat a == MkCFloat b = a == b

instance Ord CFloat where
  MkCFloat a <= MkCFloat b = a <= b
  MkCFloat a < MkCFloat b = a < b

instance Show CFloat where
  showsPrec p (MkCFloat a) = showsPrec p a

instance Read CFloat where
  readsPrec p s = [(MkCFloat x, t) | (x, t) <- readsPrec p s]

instance Num CFloat where
  MkCFloat a + MkCFloat b = MkCFloat (a + b)
  MkCFloat a - MkCFloat b = MkCFloat (a - b)
  MkCFloat a * MkCFloat b = MkCFloat (a * b)
  negate (MkCFloat a) = MkCFloat (negate a)
  abs (MkCFloat a) = MkCFloat (abs a)
  signum (MkCFloat a) = MkCFloat (signum a)
  fromInteger n = MkCFloat (fromInteger n)

instance Real CFloat where
  toRational (MkCFloat a) = toRational a

instance Fractional CFloat where
  MkCFloat a / MkCFloat b = MkCFloat (a / b)
  fromRational q = MkCFloat (fromRational q)

instance Floating CFloat where
  pi = MkCFloat pi
  exp (MkCFloat a) = MkCFloat (exp a)
  log (MkCFloat a) = MkCFloat (log a)
  sqrt (MkCFloat a) = MkCFloat (sqrt a)
  sin (MkCFloat a) = MkCFloat (sin a)
  cos (MkCFloat a) = MkCFloat (cos a)
  asin (MkCFloat a) = MkCFloat (asin a)
  acos (MkCFloat a) = MkCFloat (acos a)
  atan (MkCFloat a) = MkCFloat (atan a)
  sinh (MkCFloat a) = MkCFloat (sinh a)
  cosh (MkCFloat a) = MkCFloat (cosh a)
  asinh (MkCFloat a) = MkCFloat (asinh a)
  acosh (MkCFloat a) = MkCFloat (acosh a)
  atanh (MkCFloat a) = MkCFloat (atanh a)
  MkCFloat a ** MkCFloat b = MkCFloat (a ** b)

instance RealFrac CFloat where
  properFraction (MkCFloat a) = let (n, r) = properFraction a in (n, MkCFloat r)

instance Enum CFloat where
  succ x = x + 1
  pred x = x - 1
  toEnum n = MkCFloat (toEnum n)
  fromEnum (MkCFloat a) = fromEnum a
  enumFrom (MkCFloat a) = map MkCFloat (enumFrom a)
  enumFromThen (MkCFloat a) (MkCFloat b) = map MkCFloat (enumFromThen a b)
  enumFromTo (MkCFloat a) (MkCFloat b) = map MkCFloat (enumFromTo a b)
  enumFromThenTo (MkCFloat a) (MkCFloat b) (MkCFloat c) = map MkCFloat (enumFromThenTo a b c)

newtype CPtrdiff = MkCPtrdiff Integer

toCPtrdiff :: Integer -> CPtrdiff
toCPtrdiff x = MkCPtrdiff (mod (x + 9223372036854775808) 18446744073709551616 - 9223372036854775808)

instance Eq CPtrdiff where
  MkCPtrdiff a == MkCPtrdiff b = a == b

instance Ord CPtrdiff where
  compare (MkCPtrdiff a) (MkCPtrdiff b) = compare a b
  MkCPtrdiff a <= MkCPtrdiff b = a <= b
  MkCPtrdiff a < MkCPtrdiff b = a < b

instance Show CPtrdiff where
  showsPrec p (MkCPtrdiff a) = showsPrec p a

instance Read CPtrdiff where
  readsPrec p s = [(toCPtrdiff x, t) | (x, t) <- readsPrec p s]

instance Num CPtrdiff where
  MkCPtrdiff a + MkCPtrdiff b = toCPtrdiff (a + b)
  MkCPtrdiff a - MkCPtrdiff b = toCPtrdiff (a - b)
  MkCPtrdiff a * MkCPtrdiff b = toCPtrdiff (a * b)
  negate (MkCPtrdiff a) = toCPtrdiff (negate a)
  abs (MkCPtrdiff a) = toCPtrdiff (abs a)
  signum (MkCPtrdiff a) = MkCPtrdiff (signum a)
  fromInteger = toCPtrdiff

instance Real CPtrdiff where
  toRational (MkCPtrdiff a) = toRational a

instance Enum CPtrdiff where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toCPtrdiff (toInteger n)
  fromEnum (MkCPtrdiff a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkCPtrdiff a) (MkCPtrdiff b) = map MkCPtrdiff (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkCPtrdiff a) (MkCPtrdiff b) (MkCPtrdiff d) = map MkCPtrdiff (enumFromThenTo a b d)

instance Integral CPtrdiff where
  quot (MkCPtrdiff a) (MkCPtrdiff b) = toCPtrdiff (quot a b)
  rem (MkCPtrdiff a) (MkCPtrdiff b) = toCPtrdiff (rem a b)
  div (MkCPtrdiff a) (MkCPtrdiff b) = toCPtrdiff (div a b)
  mod (MkCPtrdiff a) (MkCPtrdiff b) = toCPtrdiff (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkCPtrdiff a) = a

instance Bounded CPtrdiff where
  minBound = MkCPtrdiff (negate 9223372036854775808)
  maxBound = MkCPtrdiff 9223372036854775807

newtype CWchar = MkCWchar Integer

toCWchar :: Integer -> CWchar
toCWchar x = MkCWchar (mod (x + 2147483648) 4294967296 - 2147483648)

instance Eq CWchar where
  MkCWchar a == MkCWchar b = a == b

instance Ord CWchar where
  compare (MkCWchar a) (MkCWchar b) = compare a b
  MkCWchar a <= MkCWchar b = a <= b
  MkCWchar a < MkCWchar b = a < b

instance Show CWchar where
  showsPrec p (MkCWchar a) = showsPrec p a

instance Read CWchar where
  readsPrec p s = [(toCWchar x, t) | (x, t) <- readsPrec p s]

instance Num CWchar where
  MkCWchar a + MkCWchar b = toCWchar (a + b)
  MkCWchar a - MkCWchar b = toCWchar (a - b)
  MkCWchar a * MkCWchar b = toCWchar (a * b)
  negate (MkCWchar a) = toCWchar (negate a)
  abs (MkCWchar a) = toCWchar (abs a)
  signum (MkCWchar a) = MkCWchar (signum a)
  fromInteger = toCWchar

instance Real CWchar where
  toRational (MkCWchar a) = toRational a

instance Enum CWchar where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toCWchar (toInteger n)
  fromEnum (MkCWchar a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkCWchar a) (MkCWchar b) = map MkCWchar (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkCWchar a) (MkCWchar b) (MkCWchar d) = map MkCWchar (enumFromThenTo a b d)

instance Integral CWchar where
  quot (MkCWchar a) (MkCWchar b) = toCWchar (quot a b)
  rem (MkCWchar a) (MkCWchar b) = toCWchar (rem a b)
  div (MkCWchar a) (MkCWchar b) = toCWchar (div a b)
  mod (MkCWchar a) (MkCWchar b) = toCWchar (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkCWchar a) = a

instance Bounded CWchar where
  minBound = MkCWchar (negate 2147483648)
  maxBound = MkCWchar 2147483647

newtype CSigAtomic = MkCSigAtomic Integer

toCSigAtomic :: Integer -> CSigAtomic
toCSigAtomic x = MkCSigAtomic (mod (x + 2147483648) 4294967296 - 2147483648)

instance Eq CSigAtomic where
  MkCSigAtomic a == MkCSigAtomic b = a == b

instance Ord CSigAtomic where
  compare (MkCSigAtomic a) (MkCSigAtomic b) = compare a b
  MkCSigAtomic a <= MkCSigAtomic b = a <= b
  MkCSigAtomic a < MkCSigAtomic b = a < b

instance Show CSigAtomic where
  showsPrec p (MkCSigAtomic a) = showsPrec p a

instance Read CSigAtomic where
  readsPrec p s = [(toCSigAtomic x, t) | (x, t) <- readsPrec p s]

instance Num CSigAtomic where
  MkCSigAtomic a + MkCSigAtomic b = toCSigAtomic (a + b)
  MkCSigAtomic a - MkCSigAtomic b = toCSigAtomic (a - b)
  MkCSigAtomic a * MkCSigAtomic b = toCSigAtomic (a * b)
  negate (MkCSigAtomic a) = toCSigAtomic (negate a)
  abs (MkCSigAtomic a) = toCSigAtomic (abs a)
  signum (MkCSigAtomic a) = MkCSigAtomic (signum a)
  fromInteger = toCSigAtomic

instance Real CSigAtomic where
  toRational (MkCSigAtomic a) = toRational a

instance Enum CSigAtomic where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toCSigAtomic (toInteger n)
  fromEnum (MkCSigAtomic a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkCSigAtomic a) (MkCSigAtomic b) = map MkCSigAtomic (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkCSigAtomic a) (MkCSigAtomic b) (MkCSigAtomic d) = map MkCSigAtomic (enumFromThenTo a b d)

instance Integral CSigAtomic where
  quot (MkCSigAtomic a) (MkCSigAtomic b) = toCSigAtomic (quot a b)
  rem (MkCSigAtomic a) (MkCSigAtomic b) = toCSigAtomic (rem a b)
  div (MkCSigAtomic a) (MkCSigAtomic b) = toCSigAtomic (div a b)
  mod (MkCSigAtomic a) (MkCSigAtomic b) = toCSigAtomic (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkCSigAtomic a) = a

instance Bounded CSigAtomic where
  minBound = MkCSigAtomic (negate 2147483648)
  maxBound = MkCSigAtomic 2147483647

newtype CClock = MkCClock Integer

toCClock :: Integer -> CClock
toCClock x = MkCClock (mod (x + 9223372036854775808) 18446744073709551616 - 9223372036854775808)

instance Eq CClock where
  MkCClock a == MkCClock b = a == b

instance Ord CClock where
  compare (MkCClock a) (MkCClock b) = compare a b
  MkCClock a <= MkCClock b = a <= b
  MkCClock a < MkCClock b = a < b

instance Show CClock where
  showsPrec p (MkCClock a) = showsPrec p a

instance Read CClock where
  readsPrec p s = [(toCClock x, t) | (x, t) <- readsPrec p s]

instance Num CClock where
  MkCClock a + MkCClock b = toCClock (a + b)
  MkCClock a - MkCClock b = toCClock (a - b)
  MkCClock a * MkCClock b = toCClock (a * b)
  negate (MkCClock a) = toCClock (negate a)
  abs (MkCClock a) = toCClock (abs a)
  signum (MkCClock a) = MkCClock (signum a)
  fromInteger = toCClock

instance Real CClock where
  toRational (MkCClock a) = toRational a

instance Enum CClock where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toCClock (toInteger n)
  fromEnum (MkCClock a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkCClock a) (MkCClock b) = map MkCClock (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkCClock a) (MkCClock b) (MkCClock d) = map MkCClock (enumFromThenTo a b d)

instance Integral CClock where
  quot (MkCClock a) (MkCClock b) = toCClock (quot a b)
  rem (MkCClock a) (MkCClock b) = toCClock (rem a b)
  div (MkCClock a) (MkCClock b) = toCClock (div a b)
  mod (MkCClock a) (MkCClock b) = toCClock (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkCClock a) = a

instance Bounded CClock where
  minBound = MkCClock (negate 9223372036854775808)
  maxBound = MkCClock 9223372036854775807

newtype CTime = MkCTime Integer

toCTime :: Integer -> CTime
toCTime x = MkCTime (mod (x + 9223372036854775808) 18446744073709551616 - 9223372036854775808)

instance Eq CTime where
  MkCTime a == MkCTime b = a == b

instance Ord CTime where
  compare (MkCTime a) (MkCTime b) = compare a b
  MkCTime a <= MkCTime b = a <= b
  MkCTime a < MkCTime b = a < b

instance Show CTime where
  showsPrec p (MkCTime a) = showsPrec p a

instance Read CTime where
  readsPrec p s = [(toCTime x, t) | (x, t) <- readsPrec p s]

instance Num CTime where
  MkCTime a + MkCTime b = toCTime (a + b)
  MkCTime a - MkCTime b = toCTime (a - b)
  MkCTime a * MkCTime b = toCTime (a * b)
  negate (MkCTime a) = toCTime (negate a)
  abs (MkCTime a) = toCTime (abs a)
  signum (MkCTime a) = MkCTime (signum a)
  fromInteger = toCTime

instance Real CTime where
  toRational (MkCTime a) = toRational a

instance Enum CTime where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toCTime (toInteger n)
  fromEnum (MkCTime a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkCTime a) (MkCTime b) = map MkCTime (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkCTime a) (MkCTime b) (MkCTime d) = map MkCTime (enumFromThenTo a b d)

instance Integral CTime where
  quot (MkCTime a) (MkCTime b) = toCTime (quot a b)
  rem (MkCTime a) (MkCTime b) = toCTime (rem a b)
  div (MkCTime a) (MkCTime b) = toCTime (div a b)
  mod (MkCTime a) (MkCTime b) = toCTime (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkCTime a) = a

instance Bounded CTime where
  minBound = MkCTime (negate 9223372036854775808)
  maxBound = MkCTime 9223372036854775807

newtype CIntPtr = MkCIntPtr Integer

toCIntPtr :: Integer -> CIntPtr
toCIntPtr x = MkCIntPtr (mod (x + 9223372036854775808) 18446744073709551616 - 9223372036854775808)

instance Eq CIntPtr where
  MkCIntPtr a == MkCIntPtr b = a == b

instance Ord CIntPtr where
  compare (MkCIntPtr a) (MkCIntPtr b) = compare a b
  MkCIntPtr a <= MkCIntPtr b = a <= b
  MkCIntPtr a < MkCIntPtr b = a < b

instance Show CIntPtr where
  showsPrec p (MkCIntPtr a) = showsPrec p a

instance Read CIntPtr where
  readsPrec p s = [(toCIntPtr x, t) | (x, t) <- readsPrec p s]

instance Num CIntPtr where
  MkCIntPtr a + MkCIntPtr b = toCIntPtr (a + b)
  MkCIntPtr a - MkCIntPtr b = toCIntPtr (a - b)
  MkCIntPtr a * MkCIntPtr b = toCIntPtr (a * b)
  negate (MkCIntPtr a) = toCIntPtr (negate a)
  abs (MkCIntPtr a) = toCIntPtr (abs a)
  signum (MkCIntPtr a) = MkCIntPtr (signum a)
  fromInteger = toCIntPtr

instance Real CIntPtr where
  toRational (MkCIntPtr a) = toRational a

instance Enum CIntPtr where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toCIntPtr (toInteger n)
  fromEnum (MkCIntPtr a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkCIntPtr a) (MkCIntPtr b) = map MkCIntPtr (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkCIntPtr a) (MkCIntPtr b) (MkCIntPtr d) = map MkCIntPtr (enumFromThenTo a b d)

instance Integral CIntPtr where
  quot (MkCIntPtr a) (MkCIntPtr b) = toCIntPtr (quot a b)
  rem (MkCIntPtr a) (MkCIntPtr b) = toCIntPtr (rem a b)
  div (MkCIntPtr a) (MkCIntPtr b) = toCIntPtr (div a b)
  mod (MkCIntPtr a) (MkCIntPtr b) = toCIntPtr (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkCIntPtr a) = a

instance Bounded CIntPtr where
  minBound = MkCIntPtr (negate 9223372036854775808)
  maxBound = MkCIntPtr 9223372036854775807

newtype CUIntPtr = MkCUIntPtr Integer

toCUIntPtr :: Integer -> CUIntPtr
toCUIntPtr x = MkCUIntPtr (mod x 18446744073709551616)

instance Eq CUIntPtr where
  MkCUIntPtr a == MkCUIntPtr b = a == b

instance Ord CUIntPtr where
  compare (MkCUIntPtr a) (MkCUIntPtr b) = compare a b
  MkCUIntPtr a <= MkCUIntPtr b = a <= b
  MkCUIntPtr a < MkCUIntPtr b = a < b

instance Show CUIntPtr where
  showsPrec p (MkCUIntPtr a) = showsPrec p a

instance Read CUIntPtr where
  readsPrec p s = [(toCUIntPtr x, t) | (x, t) <- readsPrec p s]

instance Num CUIntPtr where
  MkCUIntPtr a + MkCUIntPtr b = toCUIntPtr (a + b)
  MkCUIntPtr a - MkCUIntPtr b = toCUIntPtr (a - b)
  MkCUIntPtr a * MkCUIntPtr b = toCUIntPtr (a * b)
  negate (MkCUIntPtr a) = toCUIntPtr (negate a)
  abs (MkCUIntPtr a) = toCUIntPtr (abs a)
  signum (MkCUIntPtr a) = MkCUIntPtr (signum a)
  fromInteger = toCUIntPtr

instance Real CUIntPtr where
  toRational (MkCUIntPtr a) = toRational a

instance Enum CUIntPtr where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toCUIntPtr (toInteger n)
  fromEnum (MkCUIntPtr a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkCUIntPtr a) (MkCUIntPtr b) = map MkCUIntPtr (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkCUIntPtr a) (MkCUIntPtr b) (MkCUIntPtr d) = map MkCUIntPtr (enumFromThenTo a b d)

instance Integral CUIntPtr where
  quot (MkCUIntPtr a) (MkCUIntPtr b) = toCUIntPtr (quot a b)
  rem (MkCUIntPtr a) (MkCUIntPtr b) = toCUIntPtr (rem a b)
  div (MkCUIntPtr a) (MkCUIntPtr b) = toCUIntPtr (div a b)
  mod (MkCUIntPtr a) (MkCUIntPtr b) = toCUIntPtr (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkCUIntPtr a) = a

instance Bounded CUIntPtr where
  minBound = MkCUIntPtr (0)
  maxBound = MkCUIntPtr 18446744073709551615

newtype CIntMax = MkCIntMax Integer

toCIntMax :: Integer -> CIntMax
toCIntMax x = MkCIntMax (mod (x + 9223372036854775808) 18446744073709551616 - 9223372036854775808)

instance Eq CIntMax where
  MkCIntMax a == MkCIntMax b = a == b

instance Ord CIntMax where
  compare (MkCIntMax a) (MkCIntMax b) = compare a b
  MkCIntMax a <= MkCIntMax b = a <= b
  MkCIntMax a < MkCIntMax b = a < b

instance Show CIntMax where
  showsPrec p (MkCIntMax a) = showsPrec p a

instance Read CIntMax where
  readsPrec p s = [(toCIntMax x, t) | (x, t) <- readsPrec p s]

instance Num CIntMax where
  MkCIntMax a + MkCIntMax b = toCIntMax (a + b)
  MkCIntMax a - MkCIntMax b = toCIntMax (a - b)
  MkCIntMax a * MkCIntMax b = toCIntMax (a * b)
  negate (MkCIntMax a) = toCIntMax (negate a)
  abs (MkCIntMax a) = toCIntMax (abs a)
  signum (MkCIntMax a) = MkCIntMax (signum a)
  fromInteger = toCIntMax

instance Real CIntMax where
  toRational (MkCIntMax a) = toRational a

instance Enum CIntMax where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toCIntMax (toInteger n)
  fromEnum (MkCIntMax a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkCIntMax a) (MkCIntMax b) = map MkCIntMax (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkCIntMax a) (MkCIntMax b) (MkCIntMax d) = map MkCIntMax (enumFromThenTo a b d)

instance Integral CIntMax where
  quot (MkCIntMax a) (MkCIntMax b) = toCIntMax (quot a b)
  rem (MkCIntMax a) (MkCIntMax b) = toCIntMax (rem a b)
  div (MkCIntMax a) (MkCIntMax b) = toCIntMax (div a b)
  mod (MkCIntMax a) (MkCIntMax b) = toCIntMax (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkCIntMax a) = a

instance Bounded CIntMax where
  minBound = MkCIntMax (negate 9223372036854775808)
  maxBound = MkCIntMax 9223372036854775807

newtype CUIntMax = MkCUIntMax Integer

toCUIntMax :: Integer -> CUIntMax
toCUIntMax x = MkCUIntMax (mod x 18446744073709551616)

instance Eq CUIntMax where
  MkCUIntMax a == MkCUIntMax b = a == b

instance Ord CUIntMax where
  compare (MkCUIntMax a) (MkCUIntMax b) = compare a b
  MkCUIntMax a <= MkCUIntMax b = a <= b
  MkCUIntMax a < MkCUIntMax b = a < b

instance Show CUIntMax where
  showsPrec p (MkCUIntMax a) = showsPrec p a

instance Read CUIntMax where
  readsPrec p s = [(toCUIntMax x, t) | (x, t) <- readsPrec p s]

instance Num CUIntMax where
  MkCUIntMax a + MkCUIntMax b = toCUIntMax (a + b)
  MkCUIntMax a - MkCUIntMax b = toCUIntMax (a - b)
  MkCUIntMax a * MkCUIntMax b = toCUIntMax (a * b)
  negate (MkCUIntMax a) = toCUIntMax (negate a)
  abs (MkCUIntMax a) = toCUIntMax (abs a)
  signum (MkCUIntMax a) = MkCUIntMax (signum a)
  fromInteger = toCUIntMax

instance Real CUIntMax where
  toRational (MkCUIntMax a) = toRational a

instance Enum CUIntMax where
  succ x = x + 1
  pred x = x - 1
  toEnum n = toCUIntMax (toInteger n)
  fromEnum (MkCUIntMax a) = fromInteger a
  enumFrom x = enumFromTo x maxBound
  enumFromTo (MkCUIntMax a) (MkCUIntMax b) = map MkCUIntMax (enumFromTo a b)
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromThenTo (MkCUIntMax a) (MkCUIntMax b) (MkCUIntMax d) = map MkCUIntMax (enumFromThenTo a b d)

instance Integral CUIntMax where
  quot (MkCUIntMax a) (MkCUIntMax b) = toCUIntMax (quot a b)
  rem (MkCUIntMax a) (MkCUIntMax b) = toCUIntMax (rem a b)
  div (MkCUIntMax a) (MkCUIntMax b) = toCUIntMax (div a b)
  mod (MkCUIntMax a) (MkCUIntMax b) = toCUIntMax (mod a b)
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger (MkCUIntMax a) = a

instance Bounded CUIntMax where
  minBound = MkCUIntMax (0)
  maxBound = MkCUIntMax 18446744073709551615

-- What C keeps to itself: there is nothing to do with one but point at it.
data CFile = CFile

data CFpos = CFpos

data CJmpBuf = CJmpBuf

-- Bits, for the integers.

instance Bits CChar where
  MkCChar a .&. MkCChar b = toCChar (a .&. b)
  MkCChar a .|. MkCChar b = toCChar (a .|. b)
  xor (MkCChar a) (MkCChar b) = toCChar (xor a b)
  complement (MkCChar a) = toCChar (complement a)
  shiftL (MkCChar a) n = toCChar (shiftL a n)
  shiftR (MkCChar a) n = toCChar (shiftR a n)
  bit n = toCChar (bit n)
  testBit (MkCChar a) n = testBit a n
  rotate (MkCChar a) n = let k = mod n 8; u = mod a 256 in toCChar (u * 2 ^ k + div u (2 ^ (8 - k)))
  bitSize _ = 8
  isSigned _ = True
  popCount (MkCChar a) = popCount (if a < 0 then a + 256 else a)

instance Bits CSChar where
  MkCSChar a .&. MkCSChar b = toCSChar (a .&. b)
  MkCSChar a .|. MkCSChar b = toCSChar (a .|. b)
  xor (MkCSChar a) (MkCSChar b) = toCSChar (xor a b)
  complement (MkCSChar a) = toCSChar (complement a)
  shiftL (MkCSChar a) n = toCSChar (shiftL a n)
  shiftR (MkCSChar a) n = toCSChar (shiftR a n)
  bit n = toCSChar (bit n)
  testBit (MkCSChar a) n = testBit a n
  rotate (MkCSChar a) n = let k = mod n 8; u = mod a 256 in toCSChar (u * 2 ^ k + div u (2 ^ (8 - k)))
  bitSize _ = 8
  isSigned _ = True
  popCount (MkCSChar a) = popCount (if a < 0 then a + 256 else a)

instance Bits CUChar where
  MkCUChar a .&. MkCUChar b = toCUChar (a .&. b)
  MkCUChar a .|. MkCUChar b = toCUChar (a .|. b)
  xor (MkCUChar a) (MkCUChar b) = toCUChar (xor a b)
  complement (MkCUChar a) = toCUChar (complement a)
  shiftL (MkCUChar a) n = toCUChar (shiftL a n)
  shiftR (MkCUChar a) n = toCUChar (shiftR a n)
  bit n = toCUChar (bit n)
  testBit (MkCUChar a) n = testBit a n
  rotate (MkCUChar a) n = let k = mod n 8; u = mod a 256 in toCUChar (u * 2 ^ k + div u (2 ^ (8 - k)))
  bitSize _ = 8
  isSigned _ = False
  popCount (MkCUChar a) = popCount a

instance Bits CShort where
  MkCShort a .&. MkCShort b = toCShort (a .&. b)
  MkCShort a .|. MkCShort b = toCShort (a .|. b)
  xor (MkCShort a) (MkCShort b) = toCShort (xor a b)
  complement (MkCShort a) = toCShort (complement a)
  shiftL (MkCShort a) n = toCShort (shiftL a n)
  shiftR (MkCShort a) n = toCShort (shiftR a n)
  bit n = toCShort (bit n)
  testBit (MkCShort a) n = testBit a n
  rotate (MkCShort a) n = let k = mod n 16; u = mod a 65536 in toCShort (u * 2 ^ k + div u (2 ^ (16 - k)))
  bitSize _ = 16
  isSigned _ = True
  popCount (MkCShort a) = popCount (if a < 0 then a + 65536 else a)

instance Bits CUShort where
  MkCUShort a .&. MkCUShort b = toCUShort (a .&. b)
  MkCUShort a .|. MkCUShort b = toCUShort (a .|. b)
  xor (MkCUShort a) (MkCUShort b) = toCUShort (xor a b)
  complement (MkCUShort a) = toCUShort (complement a)
  shiftL (MkCUShort a) n = toCUShort (shiftL a n)
  shiftR (MkCUShort a) n = toCUShort (shiftR a n)
  bit n = toCUShort (bit n)
  testBit (MkCUShort a) n = testBit a n
  rotate (MkCUShort a) n = let k = mod n 16; u = mod a 65536 in toCUShort (u * 2 ^ k + div u (2 ^ (16 - k)))
  bitSize _ = 16
  isSigned _ = False
  popCount (MkCUShort a) = popCount a

instance Bits CInt where
  MkCInt a .&. MkCInt b = toCInt (a .&. b)
  MkCInt a .|. MkCInt b = toCInt (a .|. b)
  xor (MkCInt a) (MkCInt b) = toCInt (xor a b)
  complement (MkCInt a) = toCInt (complement a)
  shiftL (MkCInt a) n = toCInt (shiftL a n)
  shiftR (MkCInt a) n = toCInt (shiftR a n)
  bit n = toCInt (bit n)
  testBit (MkCInt a) n = testBit a n
  rotate (MkCInt a) n = let k = mod n 32; u = mod a 4294967296 in toCInt (u * 2 ^ k + div u (2 ^ (32 - k)))
  bitSize _ = 32
  isSigned _ = True
  popCount (MkCInt a) = popCount (if a < 0 then a + 4294967296 else a)

instance Bits CUInt where
  MkCUInt a .&. MkCUInt b = toCUInt (a .&. b)
  MkCUInt a .|. MkCUInt b = toCUInt (a .|. b)
  xor (MkCUInt a) (MkCUInt b) = toCUInt (xor a b)
  complement (MkCUInt a) = toCUInt (complement a)
  shiftL (MkCUInt a) n = toCUInt (shiftL a n)
  shiftR (MkCUInt a) n = toCUInt (shiftR a n)
  bit n = toCUInt (bit n)
  testBit (MkCUInt a) n = testBit a n
  rotate (MkCUInt a) n = let k = mod n 32; u = mod a 4294967296 in toCUInt (u * 2 ^ k + div u (2 ^ (32 - k)))
  bitSize _ = 32
  isSigned _ = False
  popCount (MkCUInt a) = popCount a

instance Bits CLong where
  MkCLong a .&. MkCLong b = toCLong (a .&. b)
  MkCLong a .|. MkCLong b = toCLong (a .|. b)
  xor (MkCLong a) (MkCLong b) = toCLong (xor a b)
  complement (MkCLong a) = toCLong (complement a)
  shiftL (MkCLong a) n = toCLong (shiftL a n)
  shiftR (MkCLong a) n = toCLong (shiftR a n)
  bit n = toCLong (bit n)
  testBit (MkCLong a) n = testBit a n
  rotate (MkCLong a) n = let k = mod n 64; u = mod a 18446744073709551616 in toCLong (u * 2 ^ k + div u (2 ^ (64 - k)))
  bitSize _ = 64
  isSigned _ = True
  popCount (MkCLong a) = popCount (if a < 0 then a + 18446744073709551616 else a)

instance Bits CULong where
  MkCULong a .&. MkCULong b = toCULong (a .&. b)
  MkCULong a .|. MkCULong b = toCULong (a .|. b)
  xor (MkCULong a) (MkCULong b) = toCULong (xor a b)
  complement (MkCULong a) = toCULong (complement a)
  shiftL (MkCULong a) n = toCULong (shiftL a n)
  shiftR (MkCULong a) n = toCULong (shiftR a n)
  bit n = toCULong (bit n)
  testBit (MkCULong a) n = testBit a n
  rotate (MkCULong a) n = let k = mod n 64; u = mod a 18446744073709551616 in toCULong (u * 2 ^ k + div u (2 ^ (64 - k)))
  bitSize _ = 64
  isSigned _ = False
  popCount (MkCULong a) = popCount a

instance Bits CLLong where
  MkCLLong a .&. MkCLLong b = toCLLong (a .&. b)
  MkCLLong a .|. MkCLLong b = toCLLong (a .|. b)
  xor (MkCLLong a) (MkCLLong b) = toCLLong (xor a b)
  complement (MkCLLong a) = toCLLong (complement a)
  shiftL (MkCLLong a) n = toCLLong (shiftL a n)
  shiftR (MkCLLong a) n = toCLLong (shiftR a n)
  bit n = toCLLong (bit n)
  testBit (MkCLLong a) n = testBit a n
  rotate (MkCLLong a) n = let k = mod n 64; u = mod a 18446744073709551616 in toCLLong (u * 2 ^ k + div u (2 ^ (64 - k)))
  bitSize _ = 64
  isSigned _ = True
  popCount (MkCLLong a) = popCount (if a < 0 then a + 18446744073709551616 else a)

instance Bits CULLong where
  MkCULLong a .&. MkCULLong b = toCULLong (a .&. b)
  MkCULLong a .|. MkCULLong b = toCULLong (a .|. b)
  xor (MkCULLong a) (MkCULLong b) = toCULLong (xor a b)
  complement (MkCULLong a) = toCULLong (complement a)
  shiftL (MkCULLong a) n = toCULLong (shiftL a n)
  shiftR (MkCULLong a) n = toCULLong (shiftR a n)
  bit n = toCULLong (bit n)
  testBit (MkCULLong a) n = testBit a n
  rotate (MkCULLong a) n = let k = mod n 64; u = mod a 18446744073709551616 in toCULLong (u * 2 ^ k + div u (2 ^ (64 - k)))
  bitSize _ = 64
  isSigned _ = False
  popCount (MkCULLong a) = popCount a

instance Bits CSize where
  MkCSize a .&. MkCSize b = toCSize (a .&. b)
  MkCSize a .|. MkCSize b = toCSize (a .|. b)
  xor (MkCSize a) (MkCSize b) = toCSize (xor a b)
  complement (MkCSize a) = toCSize (complement a)
  shiftL (MkCSize a) n = toCSize (shiftL a n)
  shiftR (MkCSize a) n = toCSize (shiftR a n)
  bit n = toCSize (bit n)
  testBit (MkCSize a) n = testBit a n
  rotate (MkCSize a) n = let k = mod n 64; u = mod a 18446744073709551616 in toCSize (u * 2 ^ k + div u (2 ^ (64 - k)))
  bitSize _ = 64
  isSigned _ = False
  popCount (MkCSize a) = popCount a

instance Bits CPtrdiff where
  MkCPtrdiff a .&. MkCPtrdiff b = toCPtrdiff (a .&. b)
  MkCPtrdiff a .|. MkCPtrdiff b = toCPtrdiff (a .|. b)
  xor (MkCPtrdiff a) (MkCPtrdiff b) = toCPtrdiff (xor a b)
  complement (MkCPtrdiff a) = toCPtrdiff (complement a)
  shiftL (MkCPtrdiff a) n = toCPtrdiff (shiftL a n)
  shiftR (MkCPtrdiff a) n = toCPtrdiff (shiftR a n)
  bit n = toCPtrdiff (bit n)
  testBit (MkCPtrdiff a) n = testBit a n
  rotate (MkCPtrdiff a) n = let k = mod n 64; u = mod a 18446744073709551616 in toCPtrdiff (u * 2 ^ k + div u (2 ^ (64 - k)))
  bitSize _ = 64
  isSigned _ = True
  popCount (MkCPtrdiff a) = popCount (if a < 0 then a + 18446744073709551616 else a)

instance Bits CWchar where
  MkCWchar a .&. MkCWchar b = toCWchar (a .&. b)
  MkCWchar a .|. MkCWchar b = toCWchar (a .|. b)
  xor (MkCWchar a) (MkCWchar b) = toCWchar (xor a b)
  complement (MkCWchar a) = toCWchar (complement a)
  shiftL (MkCWchar a) n = toCWchar (shiftL a n)
  shiftR (MkCWchar a) n = toCWchar (shiftR a n)
  bit n = toCWchar (bit n)
  testBit (MkCWchar a) n = testBit a n
  rotate (MkCWchar a) n = let k = mod n 32; u = mod a 4294967296 in toCWchar (u * 2 ^ k + div u (2 ^ (32 - k)))
  bitSize _ = 32
  isSigned _ = True
  popCount (MkCWchar a) = popCount (if a < 0 then a + 4294967296 else a)

instance Bits CSigAtomic where
  MkCSigAtomic a .&. MkCSigAtomic b = toCSigAtomic (a .&. b)
  MkCSigAtomic a .|. MkCSigAtomic b = toCSigAtomic (a .|. b)
  xor (MkCSigAtomic a) (MkCSigAtomic b) = toCSigAtomic (xor a b)
  complement (MkCSigAtomic a) = toCSigAtomic (complement a)
  shiftL (MkCSigAtomic a) n = toCSigAtomic (shiftL a n)
  shiftR (MkCSigAtomic a) n = toCSigAtomic (shiftR a n)
  bit n = toCSigAtomic (bit n)
  testBit (MkCSigAtomic a) n = testBit a n
  rotate (MkCSigAtomic a) n = let k = mod n 32; u = mod a 4294967296 in toCSigAtomic (u * 2 ^ k + div u (2 ^ (32 - k)))
  bitSize _ = 32
  isSigned _ = True
  popCount (MkCSigAtomic a) = popCount (if a < 0 then a + 4294967296 else a)

instance Bits CIntPtr where
  MkCIntPtr a .&. MkCIntPtr b = toCIntPtr (a .&. b)
  MkCIntPtr a .|. MkCIntPtr b = toCIntPtr (a .|. b)
  xor (MkCIntPtr a) (MkCIntPtr b) = toCIntPtr (xor a b)
  complement (MkCIntPtr a) = toCIntPtr (complement a)
  shiftL (MkCIntPtr a) n = toCIntPtr (shiftL a n)
  shiftR (MkCIntPtr a) n = toCIntPtr (shiftR a n)
  bit n = toCIntPtr (bit n)
  testBit (MkCIntPtr a) n = testBit a n
  rotate (MkCIntPtr a) n = let k = mod n 64; u = mod a 18446744073709551616 in toCIntPtr (u * 2 ^ k + div u (2 ^ (64 - k)))
  bitSize _ = 64
  isSigned _ = True
  popCount (MkCIntPtr a) = popCount (if a < 0 then a + 18446744073709551616 else a)

instance Bits CUIntPtr where
  MkCUIntPtr a .&. MkCUIntPtr b = toCUIntPtr (a .&. b)
  MkCUIntPtr a .|. MkCUIntPtr b = toCUIntPtr (a .|. b)
  xor (MkCUIntPtr a) (MkCUIntPtr b) = toCUIntPtr (xor a b)
  complement (MkCUIntPtr a) = toCUIntPtr (complement a)
  shiftL (MkCUIntPtr a) n = toCUIntPtr (shiftL a n)
  shiftR (MkCUIntPtr a) n = toCUIntPtr (shiftR a n)
  bit n = toCUIntPtr (bit n)
  testBit (MkCUIntPtr a) n = testBit a n
  rotate (MkCUIntPtr a) n = let k = mod n 64; u = mod a 18446744073709551616 in toCUIntPtr (u * 2 ^ k + div u (2 ^ (64 - k)))
  bitSize _ = 64
  isSigned _ = False
  popCount (MkCUIntPtr a) = popCount a

instance Bits CIntMax where
  MkCIntMax a .&. MkCIntMax b = toCIntMax (a .&. b)
  MkCIntMax a .|. MkCIntMax b = toCIntMax (a .|. b)
  xor (MkCIntMax a) (MkCIntMax b) = toCIntMax (xor a b)
  complement (MkCIntMax a) = toCIntMax (complement a)
  shiftL (MkCIntMax a) n = toCIntMax (shiftL a n)
  shiftR (MkCIntMax a) n = toCIntMax (shiftR a n)
  bit n = toCIntMax (bit n)
  testBit (MkCIntMax a) n = testBit a n
  rotate (MkCIntMax a) n = let k = mod n 64; u = mod a 18446744073709551616 in toCIntMax (u * 2 ^ k + div u (2 ^ (64 - k)))
  bitSize _ = 64
  isSigned _ = True
  popCount (MkCIntMax a) = popCount (if a < 0 then a + 18446744073709551616 else a)

instance Bits CUIntMax where
  MkCUIntMax a .&. MkCUIntMax b = toCUIntMax (a .&. b)
  MkCUIntMax a .|. MkCUIntMax b = toCUIntMax (a .|. b)
  xor (MkCUIntMax a) (MkCUIntMax b) = toCUIntMax (xor a b)
  complement (MkCUIntMax a) = toCUIntMax (complement a)
  shiftL (MkCUIntMax a) n = toCUIntMax (shiftL a n)
  shiftR (MkCUIntMax a) n = toCUIntMax (shiftR a n)
  bit n = toCUIntMax (bit n)
  testBit (MkCUIntMax a) n = testBit a n
  rotate (MkCUIntMax a) n = let k = mod n 64; u = mod a 18446744073709551616 in toCUIntMax (u * 2 ^ k + div u (2 ^ (64 - k)))
  bitSize _ = 64
  isSigned _ = False
  popCount (MkCUIntMax a) = popCount a
