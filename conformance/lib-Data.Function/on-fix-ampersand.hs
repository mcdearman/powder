-- Data.Function: on, fix and (&), and the functions of the Prelude it exports again
import Data.Function
import Data.List (sortBy)

main :: IO ()
main = do
  putStrLn ("on: " ++ show (((+) `on` length) [1, 2 :: Int] [3]))
  putStrLn ("on-compare: " ++ show (sortBy (compare `on` snd) [(1 :: Int, 'c'), (2, 'a'), (3, 'b')]))
  putStrLn ("on-equal: " ++ show (((==) `on` (`mod` 10)) (13 :: Int) 23))
  putStrLn ("fix: " ++ show (fix (\rec n -> if n <= 1 then 1 else n * rec (n - 1)) (10 :: Integer)))
  putStrLn ("fix-lazy: " ++ show (take 5 (fix (1 :)) :: [Int]))
  putStrLn ("ampersand: " ++ show ((5 :: Int) & (+ 1) & show))
  putStrLn ("ampersand-low: " ++ show ([1, 2, 3 :: Int] & map (* 2) & sum))
  putStrLn ("prelude: " ++ show (id 'i', const 'c' 'd', (not . not) True, flip (-) 1 (2 :: Int), negate $ (1 :: Int)))
