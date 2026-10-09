-- Haskell 2010 Report 15 (Data.Bits): bitSize (a Bits class method in the Report; deprecated and then
-- removed from the class in recent GHC base libraries, hence -ghc-differs)
module Main (main) where

import Data.Bits
import Data.Int
import Data.Word

main :: IO ()
main = do
  print ("Int", bitSize (0 :: Int))
  print ("Int8", bitSize (0 :: Int8))
  print ("Int16", bitSize (0 :: Int16))
  print ("Int32", bitSize (0 :: Int32))
  print ("Int64", bitSize (0 :: Int64))
  print ("Word", bitSize (0 :: Word))
  print ("Word8", bitSize (0 :: Word8))
  print ("Word16", bitSize (0 :: Word16))
  print ("Word32", bitSize (0 :: Word32))
  print ("Word64", bitSize (0 :: Word64))
  print ("argument is not evaluated", bitSize (undefined :: Int16))
