-- Haskell 2010 Report 13 (Control.Monad): forever looping over stdin until the EOF error is caught
module Main (main) where

import Control.Monad
import System.IO.Error

loop :: IO ()
loop = forever (getLine >>= \l -> putStrLn ("line: " ++ l))

main :: IO ()
main = do
  catchIOError loop (\e -> putStrLn ("stopped at eof: " ++ show (isEOFError e)))
  putStrLn "done"
