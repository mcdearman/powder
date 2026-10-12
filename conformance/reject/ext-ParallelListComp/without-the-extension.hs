-- ParallelListComp is not the Report's: without the extension this is refused
main :: IO ()
main = print [(x, y) | x <- [1, 2 :: Int] | y <- "ab"]
