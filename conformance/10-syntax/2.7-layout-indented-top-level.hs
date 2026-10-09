-- Haskell 2010 Report 10.3: the top-level block's indentation is set by its first lexeme, which need not be in column 1
   module Main (main) where

   import Data.Char (toUpper)

   shout :: String -> String
   shout = map toUpper

   data Colour = Red | Green
     deriving Show

   main :: IO ()
   main = do
     print ("shout", shout "abc")
     print ("colour", [Red, Green])
