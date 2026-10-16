{-# LANGUAGE GHC2021 #-}
-- An exception that nothing catches, raised while a value is worked out, ends the program with status 1, after
-- what was written before it
module Main (main) where

import Control.Exception

data Escaped = Escaped deriving (Show)

instance Exception Escaped

main :: IO ()
main = do
  putStrLn "before"
  r <- try (evaluate (throw Escaped :: Int))
  putStrLn (either (\Escaped -> "caught once") show r)
  print (length (show (throw Escaped :: Int)))
  putStrLn "never"
