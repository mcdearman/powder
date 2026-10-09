-- Haskell 2010 Report 7.1: readFile reads lazily; once fully consumed (forced with length) the file can be rewritten
path :: FilePath
path = "/tmp/powder-conformance-7.1-files-lazy-readFile-then-overwrite"

increment :: IO Int
increment = do
  s <- readFile path
  let n = read s :: Int
  n `seq` length s `seq` writeFile path (show (n + 1))
  return (n + 1)

main :: IO ()
main = do
  writeFile path "first\n"
  s <- readFile path
  length s `seq` writeFile path "second\n"
  print ("old contents survive in memory", s)
  t <- readFile path
  length t `seq` return ()
  print ("new contents on disk", t)
  writeFile path (map succ t)
  u <- readFile path
  print ("rewritten from its own old contents", length u `seq` u)
  writeFile path "0"
  a <- increment
  b <- increment
  c <- increment
  print ("read-modify-write counter", a, b, c)
  v <- readFile path
  print ("counter file", length v `seq` v)
  writeFile path (unlines (map show [1 .. 1000 :: Int]))
  w <- readFile path
  let total = sum (map read (lines w)) :: Int
  total `seq` writeFile path (show total)
  x <- readFile path
  print ("sum computed lazily then stored", length x `seq` x)
