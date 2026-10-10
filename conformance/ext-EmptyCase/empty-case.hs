{-# LANGUAGE EmptyCase #-}
-- EmptyCase: a case with no alternatives, for a type with no values
module Main (main) where

data Void

absurd :: Void -> a
absurd v = case v of {}

fromRight :: Either Void b -> b
fromRight (Right b) = b
fromRight (Left v) = absurd v

main :: IO ()
main = do
  print (fromRight (Right 'r'))
  print (map (either absurd succ) [Right 1, Right (2 :: Int)])
  print (length [absurd, absurd] :: Int)
