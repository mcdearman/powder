-- Haskell 2010 Report 41.8.2 (System.IO): readLn reads a line and parses it with readIO; readIO raises a
-- user error (isUserError) when the string does not parse
module Main (main) where

import System.IO
import System.IO.Error

main :: IO ()
main = do
  n <- readLn :: IO Int
  print ("Int", n, n + 1)
  xs <- readLn :: IO [Int]
  print ("list", xs, sum xs)
  t <- readLn :: IO (Bool, Char)
  print ("tuple", t)
  d <- readLn :: IO Double
  print ("Double with surrounding spaces", d)
  s <- readLn :: IO String
  print ("String", s)
  m <- readLn :: IO (Maybe Integer)
  print ("Maybe Integer", m)
  bad <- catchIOError (fmap show (readLn :: IO Int)) (\e -> return (if isUserError e then "user error for an unparsable line" else "other error"))
  putStrLn bad
  next <- readLn :: IO Int
  print ("the unparsable line was consumed", next)
  a <- readIO "  7 " :: IO Int
  print ("readIO", a)
  b <- readIO "[(1,\"one\"),(2,\"two\")]" :: IO [(Int, String)]
  print ("readIO structure", b)
  c <- readIO "(-5)" :: IO Int
  print ("readIO parenthesised negative", c)
  r1 <- catchIOError (fmap show (readIO "[1,2" :: IO [Int])) (\e -> return (if isUserError e then "user error for incomplete input" else "other error"))
  putStrLn r1
  r2 <- catchIOError (fmap show (readIO "12 34" :: IO Int)) (\e -> return (if isUserError e then "user error for trailing garbage" else "other error"))
  putStrLn r2
  r3 <- catchIOError (fmap show (readIO "" :: IO Int)) (\e -> return (if isUserError e then "user error for empty input" else "other error"))
  putStrLn r3
  r4 <- tryIOError (readIO "True" :: IO Bool)
  print ("tryIOError around readIO", either (const Nothing) Just r4)
  eof <- catchIOError (fmap show (readLn :: IO Int)) (\e -> return (if isEOFError e then "EOF error from readLn at end of input" else "other error"))
  putStrLn eof
