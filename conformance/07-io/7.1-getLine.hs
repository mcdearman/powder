-- Haskell 2010 Report 7.1: getLine reads a line from standard input without its newline
main :: IO ()
main = do
  a <- getLine
  b <- getLine
  c <- getLine
  d <- getLine
  e <- getLine
  print a
  print b
  print c
  print d
  print e
  print ("lengths", map length [a, b, c, d, e])
  putStrLn (reverse a)
  print ("no newline kept", '\n' `elem` concat [a, b, c, d, e])
  print ("words of the third line", words c)
