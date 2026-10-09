-- Haskell 2010 Report 9 (PreludeIO): getChar and getLine reading stdin (see the sibling .in file)
module Main (main) where

getChars :: Int -> IO String
getChars n = mapM (const getChar) [1 .. n]

main :: IO ()
main = do
  c1 <- getChar
  c2 <- getChar
  print ("two chars", c1, c2)
  rest <- getLine
  print ("rest of first line", rest)
  l2 <- getLine
  print ("second line", l2, length l2)
  l3 <- getLine
  print ("empty third line", l3, null l3)
  l4 <- getLine
  print ("line with spaces and tab", l4)
  nl <- getChars 3
  print ("getChar sees newline", nl)
  ws <- fmap words getLine
  print ("words of a line", ws)
  n <- fmap read getLine
  print ("number", n + (1 :: Int))
  [a, b] <- mapM (const getLine) [1, 2 :: Int]
  print ("two lines", a, b)
  l <- getLine
  print ("last line", l)
