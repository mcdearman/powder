-- Haskell 2010 Report 10.3: an explicit open brace must be matched by an explicit close brace; layout never closes it
module Main (main) where

main :: IO ()
main = do { putStrLn "a"
          ; putStrLn "b"
