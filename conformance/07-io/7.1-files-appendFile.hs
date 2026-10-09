-- Haskell 2010 Report 7.1: appendFile adds to the end of a file (creating it if needed); writeFile starts afresh
path :: FilePath
path = "/tmp/powder-conformance-7.1-files-appendFile"

slurp :: FilePath -> IO String
slurp p = do
  s <- readFile p
  length s `seq` return s

main :: IO ()
main = do
  writeFile path ""
  appendFile path "one\n"
  a <- slurp path
  print ("after first append", a)
  appendFile path "two\n"
  b <- slurp path
  print ("after second append", b)
  appendFile path ""
  c <- slurp path
  print ("appending the empty string changes nothing", c == b)
  appendFile path "no newline"
  appendFile path ", continued\n"
  d <- slurp path
  print ("appends join exactly", lines d)
  mapM_ (\n -> appendFile path (show n ++ "\n")) [1 .. 5 :: Int]
  e <- slurp path
  print ("appended in a loop", length (lines e), last (lines e))
  writeFile path "fresh\n"
  appendFile path "start\n"
  f <- slurp path
  print ("writeFile discards earlier appends", f)
  r <- appendFile path "x"
  print ("appendFile returns unit", r)
  g <- slurp path
  print ("final size", length g)
