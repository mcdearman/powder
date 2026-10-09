-- Haskell 2010 Report 4.5.2: a definition's type is finite; a list cannot be its own element
loop xs = xs : xs

main :: IO ()
main = return ()
