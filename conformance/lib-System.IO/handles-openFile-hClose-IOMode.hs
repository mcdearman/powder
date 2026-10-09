-- Haskell 2010 Report 41.3 (System.IO): Handle, openFile with every IOMode (ReadMode, WriteMode,
-- AppendMode, ReadWriteMode), hClose, hIsOpen, hIsClosed, hIsReadable, hIsWritable, hIsSeekable
module Main (main) where

import System.IO

path :: FilePath
path = "/tmp/powder-conformance-handles-openFile-hClose-IOMode"

slurp :: IO String
slurp = do
  s <- readFile path
  length s `seq` return s

properties :: String -> Handle -> IO ()
properties name h = do
  o <- hIsOpen h
  c <- hIsClosed h
  r <- hIsReadable h
  w <- hIsWritable h
  putStrLn (name ++ ": open=" ++ show o ++ " closed=" ++ show c ++ " readable=" ++ show r ++ " writable=" ++ show w)

-- (not asked of AppendMode handles, where the answer is implementation dependent)
seekable :: String -> Handle -> IO ()
seekable name h = do
  s <- hIsSeekable h
  putStrLn (name ++ ": seekable=" ++ show s)

closedState :: String -> Handle -> IO ()
closedState name h = do
  o <- hIsOpen h
  c <- hIsClosed h
  putStrLn (name ++ " after hClose: open=" ++ show o ++ " closed=" ++ show c)

main :: IO ()
main = do
  hw <- openFile path WriteMode
  properties "WriteMode" hw
  seekable "WriteMode" hw
  hPutStr hw "hello\nworld\n"
  hClose hw
  closedState "WriteMode" hw
  hClose hw
  putStrLn "closing a closed handle has no effect"
  s1 <- slurp
  print ("written", s1)
  hr <- openFile path ReadMode
  properties "ReadMode" hr
  seekable "ReadMode" hr
  l1 <- hGetLine hr
  print ("first line", l1)
  hClose hr
  closedState "ReadMode" hr
  ha <- openFile path AppendMode
  properties "AppendMode" ha
  hPutStr ha "appended\n"
  hClose ha
  closedState "AppendMode" ha
  s2 <- slurp
  print ("AppendMode keeps old contents", s2)
  hrw <- openFile path ReadWriteMode
  properties "ReadWriteMode" hrw
  seekable "ReadWriteMode" hrw
  hPutStr hrw "J"
  hSeek hrw AbsoluteSeek 0
  l2 <- hGetLine hrw
  print ("ReadWriteMode does not truncate", l2)
  hClose hrw
  closedState "ReadWriteMode" hrw
  s3 <- slurp
  print ("after ReadWriteMode", s3)
  ht <- openFile path WriteMode
  hClose ht
  s4 <- slurp
  print ("WriteMode truncates", s4)
  ha2 <- openFile path AppendMode
  hPutStr ha2 "x"
  hClose ha2
  ha3 <- openFile path AppendMode
  hPutStr ha3 "y"
  hClose ha3
  s5 <- slurp
  print ("two appends", s5)
  h1 <- openFile path ReadMode
  h2 <- openFile path ReadMode
  c1 <- hGetChar h1
  c2 <- hGetChar h2
  c3 <- hGetChar h1
  print ("two independent readers", c1, c2, c3)
  hClose h1
  c4 <- hGetChar h2
  print ("second reader still open", c4)
  hClose h2
