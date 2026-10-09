{-# LANGUAGE Haskell2010 #-}
-- Haskell 2010 Report 12: pragmas {-# ... #-} are lexically comments; INLINE/NOINLINE/SPECIALIZE/LANGUAGE do not change semantics
module Main (main) where

{-# INLINE double #-}
double :: Int -> Int
double x = x * 2

{-# NOINLINE triple #-}
triple :: Int -> Int
triple x = x * 3

square :: Num a => a -> a
square x = x * x
{-# SPECIALIZE square :: Int -> Int #-}
{-# SPECIALIZE square :: Double -> Double #-}

class Scale a where
  scale :: a -> a
  {-# UNKNOWN_CONFORMANCE_PRAGMA an unrecognised pragma must be ignored #-}

instance Scale Int where
  {-# INLINE scale #-}
  scale = (* 10)

main :: IO ()
main = do
  print ("double", double 21)
  print ("triple", triple 3)
  print ("square", square (4 :: Int), square (1.5 :: Double), square (3 :: Integer))
  print ("scale", scale (2 :: Int))
  print ("pragma-like text in a string", "{-# INLINE x #-}")
  print ("local", localUse 5)
  where
    localUse :: Int -> Int
    localUse n = helper n + 1
      where
        {-# INLINE helper #-}
        helper = (* 2)
