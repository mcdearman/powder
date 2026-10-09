-- Haskell 2010 Report 41.3.3, 41.7.1, 41.8 (System.IO): on a file handle: hIsTerminalDevice (False),
-- hSetEcho/hGetEcho (terminal operations; their results on a file are not printed), hShow (text is
-- implementation dependent; only its non-emptiness is printed), hWaitForInput and hReady (data available)
module Main (main) where

import System.IO

path :: FilePath
path = "/tmp/powder-conformance-properties-hIsTerminalDevice-hShow-hSetEcho-hGetEcho-hWaitForInput-hReady"

main :: IO ()
main = do
  writeFile path "some data\nmore data\n"
  h <- openFile path ReadMode
  t <- hIsTerminalDevice h
  print ("hIsTerminalDevice on a file opened for reading", t)
  s <- hShow h
  print ("hShow of an open handle is non-empty", not (null s))
  r0 <- hReady h
  print ("hReady with data available", r0)
  w0 <- hWaitForInput h 0
  print ("hWaitForInput 0 with data available", w0)
  c <- hLookAhead h
  r1 <- hReady h
  w1 <- hWaitForInput h 100
  print ("after hLookAhead", c, r1, w1)
  l <- hGetLine h
  r2 <- hReady h
  print ("after hGetLine", l, r2)
  w2 <- hWaitForInput h (-1)
  print ("hWaitForInput with a negative timeout waits indefinitely", w2)
  c2 <- hGetChar h
  print ("input is not consumed by hReady or hWaitForInput", c2)
  hSetEcho h False
  _ <- hGetEcho h
  hSetEcho h True
  _ <- hGetEcho h
  putStrLn "hSetEcho and hGetEcho accepted on a file handle"
  rest <- hGetLine h
  print ("handle still usable", rest)
  hClose h
  w <- openFile path WriteMode
  tw <- hIsTerminalDevice w
  print ("hIsTerminalDevice on a file opened for writing", tw)
  s3 <- hShow w
  print ("hShow of a write handle is non-empty", not (null s3))
  hClose w
