-- Haskell 2010 Report 7.1: writeFile creates or replaces a file, readFile reads it back
path :: FilePath
path = "/tmp/powder-conformance-7.1-files-writeFile-readFile"

-- read the whole file strictly, so that it is closed before it is written again
slurp :: FilePath -> IO String
slurp p = do
  s <- readFile p
  length s `seq` return s

main :: IO ()
main = do
  writeFile path "hello file\n"
  a <- slurp path
  print ("read back", a)
  writeFile path "replaced with longer contents than before\nsecond line\n"
  b <- slurp path
  print ("replaced", b)
  writeFile path "short"
  c <- slurp path
  print ("writeFile truncates, no newline added", c, length c)
  writeFile path (unlines (map show [1 .. 5 :: Int]))
  d <- slurp path
  putStr d
  print ("lines", lines d, sum (map read (lines d) :: [Int]))
  writeFile path "tab\there \"quoted\" back\\slash\n"
  e <- slurp path
  print ("special characters", e)
  writeFile path (replicate 100000 'x' ++ "\n")
  f <- slurp path
  print ("large", length f, all (== 'x') (init f), last f == '\n')
  r <- writeFile path "result"
  print ("writeFile returns unit", r)
  g <- readFile path >>= \s -> return (map succ s)
  print ("readFile in a bind chain", g)
