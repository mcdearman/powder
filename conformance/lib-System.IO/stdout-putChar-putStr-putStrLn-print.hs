-- Haskell 2010 Report 41.7.2, 41.8.1 (System.IO): standard output with putChar, putStr, putStrLn, print
module Main (main) where

import System.IO

data Shape = Circle Double | Rect Int Int
  deriving (Show)

main :: IO ()
main = do
  putChar 'a'
  putChar 'b'
  putChar '\n'
  putStr "no newline"
  putStr ""
  putStr " ... continued\n"
  putStrLn "with newline"
  putStrLn ""
  putStrLn "two\nlines"
  putStr (unlines ["from", "unlines"])
  mapM_ putChar "char by char\n"
  mapM_ putStrLn ["x", "y", "z"]
  print (42 :: Int)
  print (-7 :: Int)
  print (3.5 :: Double)
  print 'c'
  print "string with \"quotes\" and \\ backslash"
  print "tab\tnewline\n"
  print True
  print ()
  print [1, 2, 3 :: Int]
  print (1 :: Int, 'x', "s")
  print (Just [LT, EQ, GT])
  print (Circle 1.5)
  print [Rect 2 3, Circle 0]
  print (Left 3 :: Either Int String)
  putStrLn (show (12 :: Int) ++ " is what print would write")
  putStr "tab\there\n"
  putStrLn "trailing spaces   "
  putStrLn "   leading spaces"
  putChar '\t'
  putChar 'T'
  putChar '\n'
  sequence_ [putStr (show i) >> putChar ' ' | i <- [1 .. 5 :: Int]]
  putStrLn ""
  hFlush stdout
  putStrLn "print x is putStrLn (show x):"
  print "same"
  putStrLn (show "same")
  putStr "last line has no newline"
