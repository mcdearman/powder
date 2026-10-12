-- ScopedTypeVariables is not the Report's: without the extension a pattern cannot be said to have a type
f :: Int -> Int
f (x :: Int) = x

main :: IO ()
main = return ()
