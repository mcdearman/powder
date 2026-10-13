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

-- The machines have these, of integers as two's complement without end.
instance Bits Integer where
  (.&.) = primIntAnd
  (.|.) = primIntOr
  xor = primIntXor
  complement x = negate x - 1
  shiftL x n = primIntShiftL x (toInteger n)
  shiftR x n = primIntShiftR x (toInteger n)
  bit n = primIntShiftL 1 (toInteger n)
  testBit x n = primIntAnd (primIntShiftR x (toInteger n)) 1 /= 0
  bitSize _ = error "Data.Bits.bitSize(Integer)"
  isSigned _ = True
  popCount x = if x < 0 then negate (count (negate x)) else count x
    where
      -- Sixty-four bits at a time.
      count 0 = 0
      count y = fromInteger (primBitsCount y) + count (primIntShiftR y 64)

instance Bits Int where
  a .&. b = fromInteger (primIntAnd (toInteger a) (toInteger b))
  a .|. b = fromInteger (primIntOr (toInteger a) (toInteger b))
  xor a b = fromInteger (primIntXor (toInteger a) (toInteger b))
  complement x = negate x - 1
  shiftL x n = fromInteger (primIntShiftL (toInteger x) (toInteger n))
  shiftR x n = fromInteger (primIntShiftR (toInteger x) (toInteger n))
  rotate x n = let k = mod n 64; u = mod (toInteger x) 18446744073709551616 in fromInteger (u * 2 ^ k + div u (2 ^ (64 - k)))
  bit n = fromInteger (bit n)
  testBit x n = testBit (toInteger x) n
  bitSize _ = 64
  isSigned _ = True
  popCount x = fromInteger (primBitsCount (toInteger x))
