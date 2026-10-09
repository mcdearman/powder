-- Haskell 2010 Report 7.1: interact applies a String -> String function to the whole of standard input
import Data.Char (toUpper)

process :: String -> String
process = unlines . map transform . filter (not . null) . lines

transform :: String -> String
transform l = show (length (words l)) ++ " " ++ map toUpper (reverse l)

main :: IO ()
main = do
  putStrLn "before interact"
  interact process
  putStrLn "after interact"
