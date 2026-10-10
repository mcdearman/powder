{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NoNumericUnderscores #-}
-- LANGUAGE pragmas: an extension turned on and then off is off: 1_0 is 1 applied to a variable
module Main (main) where

apply :: (Int -> Int) -> Int -> Int
apply f = f

_0 :: Int
_0 = 5

main :: IO ()
main = print (apply (+ 1) _0)
