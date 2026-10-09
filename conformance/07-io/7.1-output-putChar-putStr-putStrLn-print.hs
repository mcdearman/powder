-- Haskell 2010 Report 7.1: output functions putChar, putStr, putStrLn and print (print = putStrLn . show)
main :: IO ()
main = do
  putChar 'a'
  putChar 'b'
  putChar '\n'
  putStr "no newline, "
  putStr "then one\n"
  putStr ""
  putStrLn "line"
  putStrLn ""
  putStrLn "two\nlines"
  putStr "tab\there\n"
  putStrLn "trailing spaces   "
  print (1 :: Int)
  print "a string is shown with quotes and escapes\n"
  print 'c'
  print [1, 2, 3 :: Int]
  print (Just True, "x", 2.5 :: Double)
  print ()
  print (-5 :: Int)
  putStrLn (show "putStrLn . show is print")
  (putStrLn . show) [LT, GT]
  putStr (unlines ["a", "b", "c"])
  putStr (show (3 :: Int))
  putChar '\n'
  mapM_ putChar "chars one by one\n"
  mapM_ putStr ["x", "y", "z", "\n"]
  r <- putChar '\n'
  print r
  u <- putStrLn "putStrLn returns unit"
  print u
  v <- print "print returns unit"
  print v
  putStrLn (replicate 200 '-')
  putStr (concatMap (\n -> show n ++ " ") [1 .. 50 :: Int])
  putStrLn ""
  putStrLn "the last line ends with a newline"
