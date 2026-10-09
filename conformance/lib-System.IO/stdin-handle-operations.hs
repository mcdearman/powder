-- Haskell 2010 Report 41.2, 41.7.1 (System.IO): the standard handles stdin, stdout, stderr used
-- explicitly: hGetChar, hGetLine, hLookAhead, hIsEOF, hGetContents on stdin; handle properties
module Main (main) where

import System.IO

main :: IO ()
main = do
  ri <- hIsReadable stdin
  wi <- hIsWritable stdin
  print ("stdin readable/writable", ri, wi)
  ro <- hIsReadable stdout
  wo <- hIsWritable stdout
  print ("stdout readable/writable", ro, wo)
  re <- hIsReadable stderr
  we <- hIsWritable stderr
  print ("stderr readable/writable", re, we)
  oi <- hIsOpen stdin
  oo <- hIsOpen stdout
  oe <- hIsOpen stderr
  print ("standard handles are open", oi, oo, oe)
  ci <- hIsClosed stdin
  print ("stdin is not closed", ci)
  e0 <- hIsEOF stdin
  print ("hIsEOF stdin at start", e0)
  la <- hLookAhead stdin
  c <- hGetChar stdin
  print ("hLookAhead then hGetChar", la, c)
  l1 <- hGetLine stdin
  print ("hGetLine stdin", l1)
  l2 <- getLine
  print ("getLine continues where hGetLine stdin stopped", l2)
  c2 <- getChar
  c3 <- hGetChar stdin
  print ("getChar and hGetChar stdin share the stream", c2, c3)
  rest <- hGetContents stdin
  print ("hGetContents stdin", rest)
  print ("remaining lines", lines rest)
  hPutStrLn stdout "written with hPutStrLn stdout"
  hPutStrLn stderr "written with hPutStrLn stderr (not compared)"
  hFlush stdout
