-- Haskell 2010 Report 41.6 (System.IO): repositioning handles with hSeek and SeekMode(AbsoluteSeek,
-- RelativeSeek, SeekFromEnd), hTell, hGetPosn, hSetPosn, HandlePosn, hIsSeekable
module Main (main) where

import System.IO

path :: FilePath
path = "/tmp/powder-conformance-seeking-hSeek-hTell-hGetPosn-hSetPosn"

main :: IO ()
main = do
  writeFile path "0123456789abcdef"
  h <- openFile path ReadMode
  sk <- hIsSeekable h
  print ("file handle is seekable", sk)
  t0 <- hTell h
  print ("hTell at start", t0)
  hSeek h AbsoluteSeek 5
  c1 <- hGetChar h
  t1 <- hTell h
  print ("AbsoluteSeek 5", c1, t1)
  hSeek h RelativeSeek 2
  c2 <- hGetChar h
  t2 <- hTell h
  print ("RelativeSeek 2", c2, t2)
  hSeek h RelativeSeek (-4)
  c3 <- hGetChar h
  t3 <- hTell h
  print ("RelativeSeek -4", c3, t3)
  hSeek h RelativeSeek 0
  t4 <- hTell h
  print ("RelativeSeek 0 stays", t4)
  hSeek h SeekFromEnd (-1)
  c4 <- hGetChar h
  e1 <- hIsEOF h
  print ("SeekFromEnd -1", c4, e1)
  hSeek h SeekFromEnd (-6)
  rest <- hGetLine h
  print ("SeekFromEnd -6", rest)
  hSeek h SeekFromEnd 0
  t5 <- hTell h
  e2 <- hIsEOF h
  print ("SeekFromEnd 0", t5, e2)
  hSeek h AbsoluteSeek 0
  c5 <- hGetChar h
  print ("back to the start", c5)
  hSeek h AbsoluteSeek 10
  la <- hLookAhead h
  t6 <- hTell h
  print ("hLookAhead does not move the position", la, t6)
  hSeek h AbsoluteSeek 3
  p <- hGetPosn h
  a <- hGetChar h
  b <- hGetChar h
  hSetPosn p
  a' <- hGetChar h
  b' <- hGetChar h
  print ("hSetPosn returns to the saved position", [a, b], [a', b'])
  q <- hGetPosn h
  hSetPosn p
  t7 <- hTell h
  hSetPosn q
  t8 <- hTell h
  print ("positions via hTell", t7, t8)
  print ("HandlePosn equality", p == p, p == q)
  hClose h
  rw <- openFile path ReadWriteMode
  hSeek rw AbsoluteSeek 4
  hPutStr rw "XY"
  tw <- hTell rw
  print ("hTell after writing", tw)
  hSeek rw SeekFromEnd (-2)
  hPutStr rw "!?"
  hSeek rw AbsoluteSeek 0
  whole <- hGetLine rw
  print ("overwritten in place", whole)
  hClose rw
  w <- openFile path WriteMode
  hPutStr w "abcdef"
  hSeek w AbsoluteSeek 1
  hPutStr w "B"
  hSeek w SeekFromEnd 0
  hPutStr w "g"
  tz <- hTell w
  hClose w
  s <- readFile path
  print ("seeking on a write handle", s, tz)
