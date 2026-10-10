-- The types of C's numbers. Each whole number is an Integer kept within
-- its size, a CDouble is a Double and a CFloat a Float.
module Foreign.C.Types (CChar, CSChar, CUChar, CShort, CUShort, CInt, CUInt, CLong, CULong, CLLong, CULLong, CSize, CDouble, CFloat) where

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
