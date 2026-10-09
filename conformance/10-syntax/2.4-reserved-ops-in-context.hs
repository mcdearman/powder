-- Haskell 2010 Report 2.4: every reserved operator ( .. : :: = \ | <- -> @ ~ => ) used in its syntactic role
module Main (main) where

class Sized a where
  size :: a -> Int

instance Sized Bool where
  size _ = 1

instance Sized a => Sized [a] where
  size xs = sum (map size xs)

total :: (Sized a, Sized b) => a -> b -> Int
total a b = size a + size b

classify :: Int -> String
classify n
  | n < 0 = "neg"
  | n == 0 = "zero"
  | otherwise = "pos"

firstTwo :: [Int] -> (Int, [Int])
firstTwo whole@(x : rest@(_ : _)) = (x + length whole, rest)
firstTwo _ = (0, [])

lazyPair :: (Int, Int) -> Int
lazyPair ~(_, _) = 7

main :: IO ()
main = do
  print ("dotdot", [1 .. 5 :: Int], ['a' .. 'e'])
  print ("cons", 1 : 2 : [] :: [Int])
  print ("typesig", (3 :: Int), (2.5 :: Double))
  print ("lambda arrow", (\x -> x + 1) (1 :: Int))
  print ("guard bar", classify (-1), classify 0, classify 1)
  print ("comprehension bar and gen", [x * y | x <- [1, 2], y <- [10, 20 :: Int]])
  r <- return (5 :: Int)
  print ("do bind", r)
  print ("as pattern", firstTwo [1, 2, 3], firstTwo [1])
  print ("tilde", lazyPair undefined)
  print ("context arrow", total True [True, False])
  print ("case arrow", case Just 'x' of { Just c -> c; Nothing -> '?' })
  print ("minus and bang are not reserved", 5 - 3 :: Int, [10, 20, 30 :: Int] !! 1)
