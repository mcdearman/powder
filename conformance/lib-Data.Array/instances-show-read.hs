-- Haskell 2010 Report 14 (Data.Array): Show and Read instances (the "array bounds assocs" form, precedence 10)
module Main (main) where

import Data.Array

chars :: Array Int Char
chars = listArray (0, 2) "abc"

grid :: Array (Int, Int) Bool
grid = listArray ((0, 0), (1, 1)) [True, False, False, True]

main :: IO ()
main = do
  print chars
  putStrLn (show chars)
  print (listArray (-1, 1) [1.5, 2.5, 3.5] :: Array Int Double)
  print (listArray (1, 2) [-5, 6] :: Array Integer Int)
  print grid
  print (listArray ('a', 'c') ["x", "y", "z"] :: Array Char String)
  print (listArray (False, True) [Just 1, Nothing] :: Array Bool (Maybe Int))
  print (listArray (1, 0) [] :: Array Int Int)
  print (Just chars)
  print [chars, chars // [(1, 'B')]]
  print (chars, 1 :: Int)
  putStrLn (showsPrec 10 chars "")
  putStrLn (showsPrec 11 chars "")
  putStrLn (shows chars "<rest")
  print (listArray (0, 1) [chars, chars] :: Array Int (Array Int Char))
  print ("read", (read "array (0,2) [(0,10),(1,20),(2,30)]" :: Array Int Int) ! 1)
  print ("read with spaces", elems (read "  array (1, 2) [ (1,'x') , (2,'y') ] " :: Array Int Char))
  print ("read parenthesised", bounds (read "(array (5,6) [(5,True),(6,False)])" :: Array Int Bool))
  print ("read unordered assocs", elems (read "array (0,2) [(2,'c'),(0,'a'),(1,'b')]" :: Array Int Char))
  print ("read . show", (read (show chars) :: Array Int Char) == chars)
  print ("read . show 2d", (read (show grid) :: Array (Int, Int) Bool) == grid)
  print ("read inside Just", fmap elems (read "Just (array (0,1) [(0,1),(1,2)])" :: Maybe (Array Int Int)))
  print ("reads leaves rest", [(elems a, r) | (a, r) <- reads "array (0,0) [(0,7)] tail" :: [(Array Int Int, String)]])
  print ("reads rejects unparenthesised at prec 11", null (readsPrec 11 "array (0,0) [(0,7)]" :: [(Array Int Int, String)]))
