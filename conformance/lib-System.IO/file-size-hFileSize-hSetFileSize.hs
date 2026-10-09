-- Haskell 2010 Report 41.4 (System.IO): hFileSize and hSetFileSize (truncating and extending a file)
module Main (main) where

import System.IO

path :: FilePath
path = "/tmp/powder-conformance-file-size-hFileSize-hSetFileSize"

slurp :: IO String
slurp = do
  s <- readFile path
  length s `seq` return s

main :: IO ()
main = do
  h <- openFile path WriteMode
  s0 <- hFileSize h
  print ("size of freshly truncated file", s0)
  hPutStr h "0123456789"
  hFlush h
  s1 <- hFileSize h
  print ("size after writing ten characters", s1)
  hPutStrLn h "abc"
  hFlush h
  s2 <- hFileSize h
  print ("size counts the newline", s2)
  hClose h
  hr <- openFile path ReadMode
  s3 <- hFileSize hr
  _ <- hGetLine hr
  s4 <- hFileSize hr
  print ("size from a read handle is independent of position", s3, s4)
  hClose hr
  ht <- openFile path ReadWriteMode
  hSetFileSize ht 4
  s5 <- hFileSize ht
  print ("size after truncating to 4", s5)
  hClose ht
  c1 <- slurp
  print ("contents after truncation", c1)
  he <- openFile path ReadWriteMode
  hSetFileSize he 8
  s6 <- hFileSize he
  print ("size after extending to 8", s6)
  hClose he
  c2 <- slurp
  print ("extension is filled with zero bytes", map fromEnum c2)
  hz <- openFile path ReadWriteMode
  hSetFileSize hz 0
  s7 <- hFileSize hz
  e <- hIsEOF hz
  print ("size after truncating to 0", s7, e)
  hClose hz
  c3 <- slurp
  print ("contents after truncating to 0", c3)
  ha <- openFile path AppendMode
  hPutStr ha "xyz"
  hFlush ha
  s8 <- hFileSize ha
  print ("size from an append handle", s8)
  hClose ha
  writeFile path (replicate 100000 'q')
  hb <- openFile path ReadMode
  s9 <- hFileSize hb
  print ("size of a larger file", s9, s9 == toInteger (100000 :: Int))
  hClose hb
