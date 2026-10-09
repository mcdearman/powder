-- Haskell 2010 Report 20.10.3 (Data.List): the "generic" operations genericLength, genericTake,
-- genericDrop, genericSplitAt, genericIndex, genericReplicate at Integer, Double and sized integer types
module Main (main) where

import Data.Int
import Data.List
import Data.Word

xs :: String
xs = "abcdef"

average :: [Double] -> Double
average ds = sum ds / genericLength ds

main :: IO ()
main = do
  print ("genericLength Integer", (genericLength xs :: Integer))
  print ("genericLength Int", (genericLength xs :: Int))
  print ("genericLength Double", (genericLength xs :: Double))
  print ("genericLength Word8", (genericLength xs :: Word8))
  print ("genericLength empty", (genericLength "" :: Integer))
  print ("genericLength agrees with length", (genericLength xs :: Int) == length xs)
  print ("genericLength in arithmetic", average [1, 2, 3, 6])
  print ("genericTake Integer", genericTake (3 :: Integer) xs)
  print ("genericTake Int8", genericTake (2 :: Int8) xs)
  print ("genericTake zero", genericTake (0 :: Integer) xs)
  print ("genericTake negative", genericTake (-4 :: Integer) xs)
  print ("genericTake too many", genericTake (100 :: Integer) xs)
  print ("genericTake huge count", genericTake (10 ^ (30 :: Int) :: Integer) xs)
  print ("genericTake infinite", genericTake (3 :: Integer) [1 :: Int ..])
  print ("genericDrop Integer", genericDrop (3 :: Integer) xs)
  print ("genericDrop Word16", genericDrop (4 :: Word16) xs)
  print ("genericDrop zero", genericDrop (0 :: Integer) xs)
  print ("genericDrop negative", genericDrop (-4 :: Integer) xs)
  print ("genericDrop too many", genericDrop (100 :: Integer) xs)
  print ("genericDrop infinite", take 2 (genericDrop (5 :: Integer) [1 :: Int ..]))
  print ("genericSplitAt Integer", genericSplitAt (2 :: Integer) xs)
  print ("genericSplitAt Int64", genericSplitAt (5 :: Int64) xs)
  print ("genericSplitAt zero", genericSplitAt (0 :: Integer) xs)
  print ("genericSplitAt negative", genericSplitAt (-1 :: Integer) xs)
  print ("genericSplitAt too many", genericSplitAt (100 :: Integer) xs)
  print ("genericSplitAt is (take, drop)", genericSplitAt (4 :: Integer) xs == (genericTake (4 :: Integer) xs, genericDrop (4 :: Integer) xs))
  print ("genericIndex Integer", genericIndex xs (0 :: Integer), genericIndex xs (5 :: Integer))
  print ("genericIndex Word8", genericIndex xs (3 :: Word8))
  print ("genericIndex infinite", genericIndex [0 :: Int ..] (1000 :: Integer))
  print ("genericIndex agrees with index", all (\i -> genericIndex xs (toInteger i) == xs !! i) [0 .. 5])
  print ("genericReplicate Integer", genericReplicate (3 :: Integer) 'x')
  print ("genericReplicate Word8", genericReplicate (2 :: Word8) True)
  print ("genericReplicate zero", genericReplicate (0 :: Integer) 'x')
  print ("genericReplicate negative", genericReplicate (-2 :: Integer) 'x')
  print ("genericReplicate agrees with replicate", genericReplicate (4 :: Integer) 'q' == replicate 4 'q')
  print ("generic functions agree with Int versions", all (\n -> genericTake (toInteger n) xs == take n xs && genericDrop (toInteger n) xs == drop n xs) [-1 .. 8])
