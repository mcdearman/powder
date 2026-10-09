-- Haskell 2010 Report 9 (PreludeIO): interact passes all of stdin to a String -> String function and prints the result
-- (see the sibling .in file)
module Main (main) where

number :: [String] -> [String]
number = zipWith (\n l -> show n ++ ": " ++ l) [1 :: Int ..]

summary :: String -> String
summary s = unlines (number (map (unwords . reverse . words) ls) ++ ["lines=" ++ show (length ls), "chars=" ++ show (length s)])
  where
    ls = lines s

main :: IO ()
main = interact summary
