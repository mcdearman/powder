-- Haskell 2010 Report 5.1 / 10.3: an abbreviated module (no header) is `module Main (main) where`; first lexeme sets the layout
import Data.List (sort)

helper :: [Int] -> [Int]
helper = reverse . sort

main :: IO ()
main = do
  print ("helper", helper [3, 1, 2])
  print ("done", ())
