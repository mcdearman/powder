-- Haskell 2010 Report 41.7.1, 41.3.4 (System.IO): text input from a file handle with hGetChar, hGetLine,
-- hLookAhead, hGetContents (semi-closed handles) and hIsEOF
module Main (main) where

import System.IO

path :: FilePath
path = "/tmp/powder-conformance-input-hGetChar-hGetLine-hLookAhead-hGetContents-hIsEOF"

main :: IO ()
main = do
  writeFile path "line one\nline two\n\nlast without newline"
  h <- openFile path ReadMode
  e0 <- hIsEOF h
  print ("hIsEOF at start", e0)
  a <- hLookAhead h
  b <- hLookAhead h
  print ("hLookAhead does not consume", a, b)
  c <- hGetChar h
  d <- hGetChar h
  print ("hGetChar", c, d)
  l1 <- hGetLine h
  print ("hGetLine reads the rest of the line", l1)
  l2 <- hGetLine h
  print ("hGetLine strips the newline", l2)
  n <- hLookAhead h
  print ("hLookAhead sees a newline", n)
  l3 <- hGetLine h
  print ("hGetLine on an empty line", l3)
  e1 <- hIsEOF h
  print ("hIsEOF before last line", e1)
  l4 <- hGetLine h
  print ("hGetLine at end of file without newline returns the partial line", l4)
  e2 <- hIsEOF h
  print ("hIsEOF at end", e2)
  hClose h
  h2 <- openFile path ReadMode
  first <- hGetLine h2
  rest <- hGetContents h2
  o <- hIsOpen h2
  cl <- hIsClosed h2
  print ("semi-closed after hGetContents", o, cl)
  print ("hGetContents returns the rest", rest)
  print ("line read before hGetContents", first)
  cl2 <- hIsClosed h2
  print ("closed once the contents have been read", cl2)
  hClose h2
  h3 <- openFile path ReadMode
  whole <- hGetContents h3
  print ("whole file", length whole, length (lines whole), words whole)
  hClose h3
  writeFile path ""
  h4 <- openFile path ReadMode
  e3 <- hIsEOF h4
  nothing <- hGetContents h4
  print ("empty file", e3, nothing)
  hClose h4
  writeFile path "x"
  h5 <- openFile path ReadMode
  x <- hGetChar h5
  e4 <- hIsEOF h5
  print ("single character file", x, e4)
  hClose h5
  writeFile path "\n"
  h6 <- openFile path ReadMode
  nl <- hGetLine h6
  e5 <- hIsEOF h6
  print ("file with only a newline", nl, e5)
  hClose h6
