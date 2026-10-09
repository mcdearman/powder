-- Haskell 2010 Report 41.5 (System.IO): BufferMode(NoBuffering, LineBuffering, BlockBuffering),
-- hSetBuffering, hGetBuffering (reporting the mode just set), hFlush; Eq, Ord, Read, Show of BufferMode
module Main (main) where

import System.IO

path :: FilePath
path = "/tmp/powder-conformance-buffering-BufferMode-hSetBuffering-hGetBuffering"

modes :: [BufferMode]
modes = [NoBuffering, LineBuffering, BlockBuffering Nothing, BlockBuffering (Just 1), BlockBuffering (Just 4096)]

writeWith :: BufferMode -> IO ()
writeWith mode = do
  h <- openFile path WriteMode
  hSetBuffering h mode
  got <- hGetBuffering h
  hPutStr h "partial line, "
  hPutStrLn h "then the rest"
  hPutStr h (unlines (map show [1 .. 50 :: Int]))
  hFlush h
  hPutStr h "tail"
  hClose h
  s <- readFile path
  print ("write with", got, got == mode, length s, last (lines s))

readWith :: BufferMode -> IO ()
readWith mode = do
  h <- openFile path ReadMode
  hSetBuffering h mode
  got <- hGetBuffering h
  l <- hGetLine h
  c <- hGetChar h
  rest <- hGetContents h
  print ("read with", got, l, c, length rest)
  hClose h

main :: IO ()
main = do
  mapM_ writeWith modes
  mapM_ readWith modes
  h <- openFile path WriteMode
  hSetBuffering h NoBuffering
  m1 <- hGetBuffering h
  hSetBuffering h (BlockBuffering (Just 512))
  m2 <- hGetBuffering h
  hSetBuffering h LineBuffering
  m3 <- hGetBuffering h
  print ("mode can be changed repeatedly", m1, m2, m3)
  hClose h
  hSetBuffering stdout LineBuffering
  o1 <- hGetBuffering stdout
  print ("stdout", o1)
  hSetBuffering stdout NoBuffering
  o2 <- hGetBuffering stdout
  putStr "unbuffered "
  putStrLn "output"
  print ("stdout", o2)
  hSetBuffering stdout (BlockBuffering Nothing)
  o3 <- hGetBuffering stdout
  print ("stdout", o3)
  print ("show", modes)
  print ("showsPrec 11", showsPrec 11 (BlockBuffering (Just 8)) "", showsPrec 11 NoBuffering "")
  print ("eq", NoBuffering == NoBuffering, NoBuffering == LineBuffering, BlockBuffering Nothing == BlockBuffering (Just 1), BlockBuffering (Just 2) == BlockBuffering (Just 2))
  print ("ord constructors", NoBuffering < LineBuffering, LineBuffering < BlockBuffering Nothing, compare (BlockBuffering Nothing) NoBuffering)
  print ("ord arguments", compare (BlockBuffering Nothing) (BlockBuffering (Just 1)), compare (BlockBuffering (Just 2)) (BlockBuffering (Just 10)))
  print ("max min", maximum modes, minimum modes)
  print ("read", (read "NoBuffering" :: BufferMode), (read "LineBuffering" :: BufferMode), (read "BlockBuffering Nothing" :: BufferMode))
  print ("read with argument", (read "BlockBuffering (Just 512)" :: BufferMode), (read " ( BlockBuffering (Just 1) ) " :: BufferMode))
  print ("read . show", all (\m -> read (show m) == m) modes)
  print ("reads failure", (reads "FullBuffering" :: [(BufferMode, String)]))
