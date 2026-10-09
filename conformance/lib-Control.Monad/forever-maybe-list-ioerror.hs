-- Haskell 2010 Report 13 (Control.Monad): forever, terminated by Nothing, [] or an IOError
module Main (main) where

import Control.Monad
import System.IO.Error

main :: IO ()
main = do
  print ("maybe", (forever (Nothing :: Maybe Int) :: Maybe ()))
  print ("list", (forever ([] :: [Int]) :: [()]))
  r <- catchIOError (forever (ioError (userError "stop"))) (\e -> return (ioeGetErrorString e))
  putStrLn ("io: " ++ r)
  catchIOError
    (forever (putStrLn "body runs once" >> ioError (userError "stop2")))
    (\e -> putStrLn ("handler: " ++ ioeGetErrorString e))
  putStrLn "done"
