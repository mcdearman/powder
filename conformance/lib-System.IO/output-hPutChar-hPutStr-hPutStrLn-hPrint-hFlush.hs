-- Haskell 2010 Report 41.7.2 (System.IO): text output with hPutChar, hPutStr, hPutStrLn, hPrint, and
-- hFlush, to a file and to the standard handles stdout and stderr
module Main (main) where

import System.IO

path :: FilePath
path = "/tmp/powder-conformance-output-hPutChar-hPutStr-hPutStrLn-hPrint-hFlush"

main :: IO ()
main = do
  h <- openFile path WriteMode
  hPutChar h 'a'
  hPutChar h '\n'
  hPutStr h "no newline,"
  hPutStr h ""
  hPutStr h " continued\n"
  hPutStrLn h "with newline"
  hPutStrLn h ""
  hPrint h (42 :: Int)
  hPrint h "a string is shown with quotes"
  hPrint h (1.5 :: Double, 'c', [True, False])
  hPrint h (Just (-3 :: Int))
  mapM_ (hPutChar h) "chars one by one\n"
  mapM_ (hPrint h) [1, 2, 3 :: Int]
  hFlush h
  size <- hFileSize h
  print ("size after hFlush", size)
  hPutStr h "after flush"
  hFlush h
  hFlush h
  hClose h
  s <- readFile path
  putStr s
  putStrLn ""
  print ("line count", length (lines s))
  print ("as lines", lines s)
  hPutChar stdout 'x'
  hPutChar stdout '\n'
  hPutStr stdout "to stdout, "
  hPutStrLn stdout "explicitly"
  hPrint stdout (Just "printed")
  hFlush stdout
  hPutStrLn stderr "this goes to stderr and is not compared"
  hPrint stderr (0 :: Int)
  hFlush stderr
  putStrLn "putStrLn and hPutStrLn stdout interleave in order:"
  hPutStrLn stdout "1"
  putStrLn "2"
  hPutStr stdout "3"
  putStr "4"
  hPutChar stdout '5'
  putChar '\n'
