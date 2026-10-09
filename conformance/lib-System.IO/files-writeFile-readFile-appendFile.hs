-- Haskell 2010 Report 41 (System.IO): FilePath, writeFile, readFile, appendFile
module Main (main) where

import System.IO

path :: FilePath
path = "/tmp/powder-conformance-files-writeFile-readFile-appendFile"

-- read the whole file strictly, so the file is closed again before it is rewritten
slurp :: FilePath -> IO String
slurp p = do
  s <- readFile p
  length s `seq` return s

main :: IO ()
main = do
  writeFile path "first\n"
  s1 <- slurp path
  print ("after writeFile", s1)
  writeFile path "second version\nwith two lines\n"
  s2 <- slurp path
  print ("writeFile replaces contents", s2)
  print ("lines", lines s2)
  writeFile path ""
  s3 <- slurp path
  print ("empty file", s3, null s3)
  appendFile path "a"
  appendFile path "bc\n"
  appendFile path ""
  appendFile path "d"
  s4 <- slurp path
  print ("appendFile accumulates", s4)
  writeFile path "no trailing newline"
  s5 <- slurp path
  print ("no newline is added", s5, last s5)
  writeFile path (unlines (map show [1 .. 5 :: Int]))
  s6 <- slurp path
  print ("numbers", map read (lines s6) :: [Int])
  print ("sum", sum (map read (lines s6) :: [Int]))
  writeFile path (show [(1 :: Int, "one"), (2, "two")])
  s7 <- slurp path
  print ("show/read round trip", (read s7 :: [(Int, String)]))
  writeFile path "tab\there, quote\" and backslash\\ and \1 control\n"
  s8 <- slurp path
  print ("special characters survive", s8)
  writeFile path (concatMap (\i -> show i ++ "\n") [1 .. 5000 :: Int])
  s9 <- slurp path
  print ("large file", length s9, length (lines s9), last (lines s9))
  writeFile path "\n\n\n"
  s10 <- slurp path
  print ("only newlines", s10, length (lines s10))
  writeFile path "rewritten after everything"
  s11 <- readFile path
  putStrLn s11
  writeFile path "lazy read: only the first line is demanded here\nsecond line\n"
  s12 <- readFile path
  putStrLn (head (lines s12))
  print ("then the rest", length s12)
