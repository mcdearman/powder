-- Haskell 2010 Report 41.3 (System.IO): withFile opens a file, passes the handle to the action, and closes
-- the handle when the action finishes (normally or by an I/O error)
module Main (main) where

import System.IO
import System.IO.Error

path :: FilePath
path = "/tmp/powder-conformance-withFile"

main :: IO ()
main = do
  withFile path WriteMode (\h -> hPutStrLn h "written inside withFile" >> hPutStrLn h "second line")
  l <- withFile path ReadMode hGetLine
  print ("result of the action is returned", l)
  n <- withFile path ReadMode (\h -> hGetLine h >> hGetLine h >>= \s -> return (length s))
  print ("length of second line", n)
  escaped <- withFile path ReadMode return
  closed <- hIsClosed escaped
  open <- hIsOpen escaped
  print ("handle is closed afterwards", closed, open)
  withFile path AppendMode (\h -> hPutStr h "appended")
  ls <- withFile path ReadMode (\h -> do { a <- hGetLine h; b <- hGetLine h; c <- hGetLine h; e <- hIsEOF h; return (a, b, c, e) })
  print ("after append", ls)
  size <- withFile path ReadMode hFileSize
  print ("size", size)
  r <- tryIOError (withFile path ReadMode (\h -> hGetLine h >> ioError (userError "abort inside withFile") >> return h))
  print ("error propagates out of withFile", either ioeGetErrorString (const "no error") r)
  withFile path WriteMode (\h -> hPutStr h "the file can be reopened for writing after the failed action")
  s <- withFile path ReadMode hGetLine
  putStrLn s
  nested <- withFile path ReadMode (\h1 -> withFile path ReadMode (\h2 -> do { a <- hGetChar h1; b <- hGetChar h2; return [a, b] }))
  print ("nested withFile for reading", nested)
  withFile path ReadWriteMode (\h -> do { hPutStr h "THE"; hSeek h AbsoluteSeek 0; t <- hGetLine h; putStrLn t })
