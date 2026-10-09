-- Bits, of whole numbers as two's complement without end.
module Data.Bits
  ( Bits (..)
  )
where

infixl 8 `shift`, `shiftL`, `shiftR`, `rotate`
infixl 7 .&.
infixl 6 `xor`
infixl 5 .|.

class Eq a => Bits a where
  (.&.), (.|.), xor :: a -> a -> a
  complement :: a -> a
  shift, rotate :: a -> Int -> a
  shiftL, shiftR :: a -> Int -> a
  rotateL, rotateR :: a -> Int -> a
  bit :: Int -> a
  setBit, clearBit, complementBit :: a -> Int -> a
  testBit :: a -> Int -> Bool
  bitSize :: a -> Int
  isSigned :: a -> Bool
  popCount :: a -> Int
  shift x n = if n >= 0 then shiftL x n else shiftR x (negate n)
  rotate = shift
  rotateL x n = rotate x n
  rotateR x n = rotate x (negate n)
  setBit x n = x .|. bit n
  clearBit x n = x .&. complement (bit n)
  complementBit x n = xor x (bit n)

-- Combine two integers a bit at a time, given what the bits make.
bitwise :: (Bool -> Bool -> Bool) -> Integer -> Integer -> Integer
bitwise f = go
  where
    -- Past the last bit that differs from the sign, each is all its sign.
    go a b
      | (a == 0 || a == negate 1) && (b == 0 || b == negate 1) = if f (a /= 0) (b /= 0) then negate 1 else 0
      | otherwise = go (div a 2) (div b 2) * 2 + (if f (mod a 2 /= 0) (mod b 2 /= 0) then 1 else 0)

instance Bits Integer where
  (.&.) = bitwise (&&)
  (.|.) = bitwise (||)
  xor = bitwise (/=)
  complement x = negate x - 1
  shiftL x n = x * 2 ^ n
  shiftR x n = div x (2 ^ n)
  bit n = 2 ^ n
  testBit x n = mod (div x (2 ^ n)) 2 /= 0
  bitSize _ = error "Data.Bits.bitSize(Integer)"
  isSigned _ = True
  popCount x = if x < 0 then negate (popCount (complement x)) else count x
    where
      count 0 = 0
      count y = fromInteger (mod y 2) + count (div y 2)

instance Bits Int where
  a .&. b = fromInteger (toInteger a .&. toInteger b)
  a .|. b = fromInteger (toInteger a .|. toInteger b)
  xor a b = fromInteger (xor (toInteger a) (toInteger b))
  complement x = negate x - 1
  shiftL x n = fromInteger (shiftL (toInteger x) n)
  shiftR x n = fromInteger (shiftR (toInteger x) n)
  rotate x n = let k = mod n 64; u = mod (toInteger x) 18446744073709551616 in fromInteger (u * 2 ^ k + div u (2 ^ (64 - k)))
  bit n = fromInteger (bit n)
  testBit x n = testBit (toInteger x) n
  bitSize _ = 64
  isSigned _ = True
  popCount x = popCount (toInteger x)
