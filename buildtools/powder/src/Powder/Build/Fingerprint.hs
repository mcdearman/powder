-- | A number that stands for some text: the same text gives the same
-- number on every machine and in every run, which is what a hash made by
-- the language's own tables does not promise.
module Powder.Build.Fingerprint (Fingerprint, fingerprint, shown) where

import Data.Bits (xor)
import Data.Char (intToDigit, ord)
import Data.Word (Word64)

type Fingerprint = Word64

-- | The fingerprint of some pieces of text, in order. The pieces are
-- kept apart: @["ab", "c"]@ and @["a", "bc"]@ differ.
fingerprint :: [String] -> Fingerprint
fingerprint = foldl' piece 14695981039346656037
  where
    piece h s = step (foldl' (\acc c -> step acc (fromIntegral (ord c))) h s) 0x10FFFF1
    -- FNV-1a, a code point at a time.
    step h x = (h `xor` x) * 1099511628211

-- | A fingerprint in sixteen hexadecimal digits.
shown :: Fingerprint -> String
shown n = [intToDigit (fromIntegral ((n `div` (16 ^ i)) `mod` 16)) | i <- [15, 14 .. 0 :: Int]]
