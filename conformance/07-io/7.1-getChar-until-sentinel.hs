-- Haskell 2010 Report 7.1: getChar reads single characters (newlines included); mixing getChar and getLine
readUntilDot :: IO String
readUntilDot = do
  c <- getChar
  if c == '.'
    then return []
    else do
      rest <- readUntilDot
      return (c : rest)

countChars :: Int -> IO Int
countChars n = do
  c <- getChar
  if c == '!' then return n else countChars (n + 1)

main :: IO ()
main = do
  a <- getChar
  b <- getChar
  print ("first two", a, b)
  s <- readUntilDot
  print ("until dot", s)
  print ("newlines seen", length (filter (== '\n') s))
  c <- getChar
  print ("char after the dot", c)
  rest <- getLine
  print ("rest of that line via getLine", rest)
  n <- countChars 0
  print ("chars before the bang", n)
  nl <- getChar
  print ("newline after the bang", nl == '\n')
  final <- getLine
  print ("final line", final)
