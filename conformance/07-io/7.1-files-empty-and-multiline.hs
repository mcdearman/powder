-- Haskell 2010 Report 7.1: empty files, multi-line files, files without a final newline; two files at once
pathA :: FilePath
pathA = "/tmp/powder-conformance-7.1-files-empty-and-multiline"

pathB :: FilePath
pathB = "/tmp/powder-conformance-7.1-files-empty-and-multiline-b"

slurp :: FilePath -> IO String
slurp p = do
  s <- readFile p
  length s `seq` return s

poem :: [String]
poem = ["roses are red", "", "  violets are blue  ", "\ttabbed", "last"]

main :: IO ()
main = do
  writeFile pathA ""
  a <- slurp pathA
  print ("empty file", a, null a, lines a)
  writeFile pathA "\n"
  b <- slurp pathA
  print ("single newline", b, lines b)
  writeFile pathA (unlines poem)
  c <- slurp pathA
  print ("multi-line round trip", lines c == poem, length (lines c))
  mapM_ (\l -> putStrLn ("[" ++ l ++ "]")) (lines c)
  writeFile pathA "a\nb\nc"
  d <- slurp pathA
  print ("no final newline", d, lines d, last d)
  writeFile pathA "\n\n\n"
  e <- slurp pathA
  print ("only newlines", length e, lines e)
  writeFile pathB "other file\n"
  writeFile pathA "this file\n"
  x <- slurp pathA
  y <- slurp pathB
  print ("two independent files", x, y)
  writeFile pathB x
  z <- slurp pathB
  print ("copied", z == x)
  s1 <- readFile pathA
  s2 <- readFile pathA
  print ("same file read twice at once", s1 == s2, length s1 + length s2)
