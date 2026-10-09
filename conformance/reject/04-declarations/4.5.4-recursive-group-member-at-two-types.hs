-- Haskell 2010 Report 4.5.4: members of a mutually recursive group (without signatures) are monomorphic
-- within the group, so `ping` cannot be used at both Char and Bool inside `pong`
module Main (main) where

ping x = pong x

pong x = const x (ping 'a', ping True)

main :: IO ()
main = putStrLn "unreachable"
