import Data.List (sort)
import Data.Maybe

sort :: [a] -> [a]
sort xs = xs

main :: IO ()
main = print (sort [2, 1])
