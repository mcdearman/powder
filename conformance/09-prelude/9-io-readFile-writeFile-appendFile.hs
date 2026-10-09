-- Haskell 2010 Report 9 (PreludeIO): FilePath, writeFile (creates or truncates), appendFile, readFile
module Main (main) where

path :: FilePath
path = "/tmp/powder-conformance-9-io-readFile-writeFile-appendFile"

-- read the whole file and force it, so that the file is closed again before the next write
slurp :: FilePath -> IO String
slurp p = do
  s <- readFile p
  length s `seq` return s

main :: IO ()
main = do
  writeFile path "line one\n"
  a <- slurp path
  print ("after writeFile", a)
  appendFile path "line two\n"
  b <- slurp path
  print ("after appendFile", b)
  appendFile path "no newline at end"
  c <- slurp path
  print ("after second appendFile", lines c, length c)
  writeFile path "replaced"
  d <- slurp path
  print ("writeFile truncates", d)
  writeFile path ""
  e <- slurp path
  print ("empty file", e, null e)
  appendFile path ""
  f <- slurp path
  print ("append nothing", f)
  writeFile path (unlines (map show [1 .. 5 :: Int]))
  g <- slurp path
  print ("numbers", sum (map read (lines g) :: [Int]))
  writeFile path (show [(1 :: Int, "one"), (2, "two")])
  h <- slurp path
  print ("show/read through a file", read h :: [(Int, String)])
  writeFile path "tab\there \"quoted\" back\\slash\n"
  i <- slurp path
  putStr i
  writeFile path (replicate 10000 'x')
  appendFile path (replicate 10000 'y')
  j <- slurp path
  print ("large", length j, take 2 j, drop 19998 j)
  writeFile path ""
  putStrLn "done"
