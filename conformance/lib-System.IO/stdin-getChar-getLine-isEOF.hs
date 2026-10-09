-- Haskell 2010 Report 41.7.1, 41.8.2 (System.IO): standard input with getChar, getLine and isEOF; the
-- last input line has no terminating newline; reading at end of file raises an EOF error
module Main (main) where

import System.IO
import System.IO.Error

main :: IO ()
main = do
  e0 <- isEOF
  print ("isEOF at start", e0)
  c <- getChar
  print ("getChar", c)
  l1 <- getLine
  print ("getLine reads the rest of the line", l1)
  l2 <- getLine
  print ("getLine", l2)
  l3 <- getLine
  print ("getLine on an empty line", l3)
  c2 <- getChar
  c3 <- getChar
  print ("getChar reads the newline too", c2, c3)
  l4 <- getLine
  print ("getLine keeps leading and inner spaces", l4)
  e1 <- isEOF
  print ("isEOF before the last line", e1)
  l5 <- getLine
  print ("getLine at end of input without newline", l5)
  e2 <- isEOF
  print ("isEOF at end", e2)
  r <- catchIOError (getLine >>= \l -> return ("read " ++ l)) (\e -> return (if isEOFError e then "EOF error from getLine" else "other error"))
  putStrLn r
  r2 <- catchIOError (getChar >>= \ch -> return ("read " ++ [ch])) (\e -> return (if isEOFError e then "EOF error from getChar" else "other error"))
  putStrLn r2
  e3 <- isEOF
  print ("isEOF still true", e3)
