{-# LANGUAGE HexFloatLiterals #-}
-- HexFloatLiterals: a fraction written in hexadecimal, with a power of two after a p
module Main (main) where

main :: IO ()
main = do
  print (0x1p0 :: Double, 0x1p4 :: Double, 0x1p-4 :: Double, 0xAp+1 :: Double)
  print (0x1.8p0 :: Double, 0x0.1p0 :: Double, 0xF.FFp0 :: Double, 0x1.8 :: Double)
  print (0x1.999999999999ap-4 :: Double, 0x1.0p1023 :: Double, 0x1p-1074 :: Double)
  print (0x1.8p1 :: Float, 0x1.8p1 :: Rational)
  print (0x10 :: Int, 0xff + 1 :: Int)
