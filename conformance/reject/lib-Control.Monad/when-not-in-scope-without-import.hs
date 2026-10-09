-- Haskell 2010 Report 13, 5.3: when is exported by Control.Monad, not by the Prelude; using it without
-- importing Control.Monad is a scope error
module Main (main) where

main :: IO ()
main = when True (putStrLn "unreachable")
