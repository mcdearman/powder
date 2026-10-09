-- Haskell 2010 Report 41.3.1, 41.3.2 (System.IO): multiple-reader single-writer locking of files within
-- one program: opening a file for writing while it is open (for reading or writing) fails with an error
-- satisfying isAlreadyInUseError; several readers are allowed; hClose releases the lock
module Main (main) where

import System.IO
import System.IO.Error

path :: FilePath
path = "/tmp/powder-conformance-file-locking-single-writer"

attempt :: IOMode -> IO String
attempt mode = catchIOError (openFile path mode >>= \h -> hClose h >> return "opened") (\e -> return (if isAlreadyInUseError e then "already in use" else "other error"))

main :: IO ()
main = do
  writeFile path "contents\n"
  w <- openFile path WriteMode
  r1 <- attempt WriteMode
  print ("second writer while a writer is open", r1)
  r2 <- attempt ReadMode
  print ("reader while a writer is open", r2)
  r3 <- attempt AppendMode
  print ("appender while a writer is open", r3)
  hPutStrLn w "the first writer is unaffected"
  hClose w
  r4 <- attempt WriteMode
  print ("writer after the first writer was closed", r4)
  writeFile path "contents\n"
  rd <- openFile path ReadMode
  r5 <- attempt ReadMode
  print ("second reader while a reader is open", r5)
  r6 <- attempt WriteMode
  print ("writer while a reader is open", r6)
  l <- hGetLine rd
  print ("the reader is unaffected", l)
  hClose rd
  r7 <- attempt ReadWriteMode
  print ("read-write after all handles were closed", r7)
  s <- readFile path
  r8 <- attempt WriteMode
  print ("writer while a lazy readFile is pending", r8)
  print ("lazy contents", s)
  r9 <- attempt WriteMode
  print ("writer after the lazy contents were consumed", r9)
