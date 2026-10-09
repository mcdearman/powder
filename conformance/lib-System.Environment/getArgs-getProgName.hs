-- Haskell 2010 Report 39 (System.Environment): getArgs (run with argv exactly: one, "two words") and
-- getProgName (its value is implementation dependent, so only facts about it are printed)
module Main (main) where

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  print ("args", args)
  print ("number of args", length args)
  print ("first", head args)
  print ("second", args !! 1)
  print ("words of second", words (args !! 1))
  print ("second contains a space", ' ' `elem` (args !! 1))
  print ("lengths", map length args)
  again <- getArgs
  print ("getArgs is repeatable", again == args)
  name <- getProgName
  print ("program name is non-empty", not (null name))
  print ("program name is not an argument", name `notElem` args)
  nameAgain <- getProgName
  print ("getProgName is repeatable", nameAgain == name)
  mapM_ (\(i, a) -> putStrLn (show i ++ ": <" ++ a ++ ">")) (zip [1 :: Int ..] args)
