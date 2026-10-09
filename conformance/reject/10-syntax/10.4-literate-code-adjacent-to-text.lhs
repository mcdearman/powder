Haskell 2010 Report 10.4: in bird-track style it is an error for a program line to be adjacent to a non-blank comment line.
> module Main (main) where
>
> main :: IO ()
> main = putStrLn "unreachable"
