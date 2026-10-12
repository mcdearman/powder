-- ViewPatterns is not the Report's: without the extension this is refused
f :: [Int] -> Int
f (length -> n) = n

main :: IO ()
main = return ()
