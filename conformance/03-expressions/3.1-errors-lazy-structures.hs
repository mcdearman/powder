-- Haskell 2010 Report 3.1: non-strict evaluation; bottoms inside data structures and infinite values
module Main (main) where

data Pair = Pair Int Int

firstP :: Pair -> Int
firstP (Pair a _) = a

ones :: [Int]
ones = 1 : ones

nats :: [Integer]
nats = 0 : map (+ 1) nats

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

primes :: [Int]
primes = sieve [2 ..]
  where
    sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
    sieve [] = []

holes :: [Int]
holes = [1, undefined, 3]

main :: IO ()
main = do
  print ("ones", take 5 ones)
  print ("nats", take 6 nats)
  print ("fibs", take 12 fibs)
  print ("primes", takeWhile (< 40) primes)
  print ("field", firstP (Pair 3 undefined))
  print ("holes", head holes, holes !! 2, length holes)
  print ("last", last [undefined, undefined, 'z'])
  print ("reverse", head (reverse [undefined, 9 :: Int]))
  print ("knot", let (evens, odds) = (0 : map (+ 1) odds, map (+ 1) evens) in take 5 evens)
  print ("lookup", lookup 2 [(1, undefined), (2, "two"), (undefined, "never")])
  print ("takeWhile", takeWhile (< 3) (1 : 2 : 3 : undefined) :: [Int])
  print ("or", or (False : True : undefined))
  print ("elem", elem 3 (1 : 2 : 3 : undefined :: [Int]))
  print ("zip infinite", zip "abc" nats)
  print ("map fst", map fst [(1 :: Int, undefined :: Char), (2, undefined)])
