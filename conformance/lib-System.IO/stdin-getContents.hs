-- Haskell 2010 Report 41.8.2 (System.IO): getContents returns all of standard input lazily
module Main (main) where

import System.IO

main :: IO ()
main = do
  first <- getLine
  print ("line read before getContents", first)
  s <- getContents
  putStrLn (head (lines s))
  print ("characters", length s)
  print ("lines", length (lines s))
  print ("words", length (words s))
  print ("line lengths", map length (lines s))
  print ("last line", last (lines s))
  print ("ends with newline", last s == '\n')
  print ("numbers", sum (map read (filter (all (`elem` "0123456789")) (words s)) :: [Int]))
  mapM_ putStrLn (zipWith (\n l -> show n ++ "| " ++ l) [1 :: Int ..] (lines s))
  hPutStrLn stdout "done"
