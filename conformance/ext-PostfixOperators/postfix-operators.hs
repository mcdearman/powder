{-# LANGUAGE PostfixOperators #-}
-- PostfixOperators: (e op) is op applied to e, so op may take one argument
module Main (main) where

(!) :: Integer -> Integer
(!) n = product [1 .. n]

(%%) :: Double -> Double
(%%) v = v / 100

main :: IO ()
main = do
  print ((5 !), (0 !), (20 !))
  print ((50 %%), map (!) [1, 2, 3])
  print (((3 !) !), ((2 + 3) !))
  print ((1 +) 2, (subtract 1) 5)
