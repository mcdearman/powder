-- Haskell 2010 Report 7.1 (hGetLine): end of file after at least one character terminates the line; the partial line is returned
main :: IO ()
main = do
  a <- getLine
  print ("first line", a)
  b <- getLine
  print ("last line, no newline in the input", b)
  print ("lengths", length a, length b)
