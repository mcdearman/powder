-- Haskell 2010 Report 9 (PreludeIO): putChar, putStr, putStrLn, print
module Main (main) where

main :: IO ()
main = do
  putChar 'a'
  putChar 'b'
  putChar '\n'
  putChar ' '
  putChar '\t'
  putChar '|'
  putChar '\n'
  putStr "no newline, "
  putStr "then newline\n"
  putStr ""
  putStr "multi\nline\nstring\n"
  putStrLn "putStrLn adds a newline"
  putStrLn ""
  putStrLn "two\nlines"
  putStrLn (replicate 3 '=')
  print (1 :: Int)
  print 'c'
  print "string is shown with quotes"
  print [1.5, 2.5 :: Double]
  print (Just (-1 :: Int), "x", ())
  print ()
  mapM_ putChar "via mapM_\n"
  mapM_ putStrLn ["a", "b"]
  mapM_ print [LT, EQ, GT]
  putStr (unlines ["u1", "u2"])
  putStr (show (42 :: Int))
  putChar '\n'
  u <- putStr "result of putStr is unit: "
  print u
  v <- print "result of print is unit"
  print (v == ())
  putStrLn (take 20 (cycle "lazy "))
  putStr (concatMap show [1 .. 10 :: Int])
  putStrLn ""
  putStrLn "print x = putStrLn (show x):"
  putStrLn (show (Just "x"))
  print (Just "x")
