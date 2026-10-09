main :: IO ()
main = do
  s <- getContents
  print (length (lines s))
  print (length (words s))
  print (length s)
