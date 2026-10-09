-- Haskell 2010 Report 42.3: the handler passed to catch (catchIOError) has type IOError -> IO a; a
-- handler taking a String is a type error
module Main (main) where

import System.IO.Error

handler :: String -> IO ()
handler msg = putStrLn msg

main :: IO ()
main = catchIOError (ioError (userError "boom")) handler
