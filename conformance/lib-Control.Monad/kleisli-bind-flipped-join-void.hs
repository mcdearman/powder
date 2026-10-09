-- Haskell 2010 Report 13 (Control.Monad): (=<<), (>=>), (<=<), join, void
module Main (main) where

import Control.Monad

half :: Int -> Maybe Int
half x = if even x then Just (x `div` 2) else Nothing

dec :: Int -> Maybe Int
dec x = if x > 0 then Just (x - 1) else Nothing

pair :: Int -> [Int]
pair x = [x, x + 1]

tens :: Int -> [Int]
tens y = [y * 10]

main :: IO ()
main = do
  print ("=<< maybe", half =<< Just 10)
  print ("=<< nothing", half =<< Nothing)
  print ("=<< list", pair =<< [1, 5])
  print ("=<< chained", dec =<< half =<< Just 8)
  print (">=> ok", (half >=> dec) 8)
  print (">=> first fails", (half >=> dec) 7)
  print (">=> second fails", (half >=> dec) 0)
  print ("<=< order", (half <=< dec) 9)
  print ("<=< vs >=>", ((half <=< dec) 9, (half >=> dec) 9))
  print (">=> list", (pair >=> tens) 1)
  print ("<=< list", (pair <=< tens) 1)
  print (">=> return left", (return >=> half) 6 == half 6)
  print (">=> return right", (half >=> return) 6 == half 6)
  print (">=> assoc", ((half >=> half) >=> dec) 8 == (half >=> (half >=> dec)) 8)
  r <- ((\x -> return (x + 1)) >=> (\y -> return (y * 2))) (3 :: Int)
  print (">=> io", r)
  print ("join lists", join [[1], [2, 3], [], [4 :: Int]])
  print ("join strings", join ["ab", "cd"])
  print ("join just just", join (Just (Just 'x')))
  print ("join just nothing", join (Just (Nothing :: Maybe Char)))
  print ("join nothing", join (Nothing :: Maybe (Maybe Char)))
  s <- join (return (putStrLn "inner action" >> return "joined"))
  print ("join io", s)
  print ("void just", void (Just (3 :: Int)))
  print ("void nothing", void (Nothing :: Maybe Int))
  print ("void list", void "abc")
  u <- void (putStrLn "voided effect" >> return (3 :: Int))
  print ("void io", u)
