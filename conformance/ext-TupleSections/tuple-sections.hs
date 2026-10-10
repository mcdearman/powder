{-# LANGUAGE TupleSections #-}
-- TupleSections: a tuple with places left out is a function of those places
module Main (main) where

main :: IO ()
main = do
  print (map (,True) "ab")
  print (map (1 :: Int,) "xy")
  print (zipWith3 (,,) [1 :: Int, 2] "ab" [True, False])
  print (map (,'m',) [1 :: Int, 2] <*>> [False])
  print ((,,) 1 2 3 :: (Int, Int, Int), (,) 'a' 'b')
  print ((1 :: Int, , 3 :: Int) 'x', (, , "z") 'p' 'q')
  print (fmap (, ()) (Just 'j'))
  print (((1 :: Int) + 2,) (not True))
  print (let pair = (,) in pair 'a' (), (succ 'a', ) 0 :: (Char, Int))
  where
    fs <*>> xs = [f x | f <- fs, x <- xs]
