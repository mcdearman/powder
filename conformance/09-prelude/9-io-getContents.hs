-- Haskell 2010 Report 9 (PreludeIO): getContents returns all remaining stdin as one (lazy) string (see the sibling .in file)
module Main (main) where

main :: IO ()
main = do
  first <- getLine
  print ("consumed before getContents", first)
  s <- getContents
  let ls = lines s
  print ("line count", length ls)
  print ("char count", length s)
  print ("ends with newline", last s == '\n')
  print ("lines", ls)
  print ("word count", length (words s))
  print ("numbers", sum (map read (words (ls !! 1)) :: [Int]))
  print ("blank lines", length (filter null ls))
  print ("longest", maximum (map length ls))
  putStr (unlines (map reverse ls))
