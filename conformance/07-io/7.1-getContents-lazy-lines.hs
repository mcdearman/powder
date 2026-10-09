-- Haskell 2010 Report 7.1: getContents returns all remaining input as a lazily read string
numbered :: [String] -> [String]
numbered ls = zipWith (\n l -> show n ++ ": " ++ l) [(1 :: Int) ..] ls

main :: IO ()
main = do
  header <- getLine
  putStrLn ("header via getLine: " ++ header)
  s <- getContents
  let ls = lines s
  mapM_ putStrLn (numbered ls)
  print ("line count", length ls)
  print ("char count including newlines", length s)
  print ("word count", length (words s))
  print ("longest line", maximum (map length ls))
  print ("sum of numbers on the last line", sum (map read (words (last ls)) :: [Int]))
  print ("blank lines", length (filter null ls))
  print ("ends with newline", last s == '\n')
