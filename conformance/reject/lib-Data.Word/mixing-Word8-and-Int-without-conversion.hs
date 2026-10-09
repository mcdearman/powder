-- Haskell 2010 Report 23, 6.4: there are no implicit numeric conversions; a Word8 and an Int cannot be
-- added without fromIntegral
module Main (main) where

import Data.Word

byte :: Word8
byte = 5

count :: Int
count = 7

main :: IO ()
main = print (byte + count)
