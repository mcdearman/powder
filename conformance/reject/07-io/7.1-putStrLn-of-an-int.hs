-- Haskell 2010 Report 7.1: putStrLn :: String -> IO () does not accept an Int (print or show is required)
main :: IO ()
main = putStrLn (42 :: Int)
