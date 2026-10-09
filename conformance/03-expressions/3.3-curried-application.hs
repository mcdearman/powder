-- Haskell 2010 Report 3.3: function application is left associative and curried
module Main (main) where

add3 :: Int -> Int -> Int -> Int
add3 a b c = a * 100 + b * 10 + c

twice :: (a -> a) -> a -> a
twice f x = f (f x)

adder :: Int -> (Int -> Int)
adder n = \m -> n + m

compose3 :: (c -> d) -> (b -> c) -> (a -> b) -> a -> d
compose3 f g h x = f (g (h x))

main :: IO ()
main = do
  print ("full", add3 1 2 3)
  print ("explicit parens", ((add3 1) 2) 3)
  print ("partial 1", map (add3 1 2) [3, 4])
  print ("partial 2", zipWith (add3 9) [1, 2] [3, 4])
  print ("binds tighter", negate 3 + 4, succ 1 * 2)
  print ("argument parens", negate (3 + 4), succ (1 * 2))
  print ("twice", twice (+ 3) 10, twice twice (* 2) 1)
  print ("returns function", adder 1 2, (adder 10) 5)
  print ("id id", id id 3, id (id 3))
  print ("const id", const id 1 2)
  print ("flip", flip add3 1 2 3)
  print ("curry", curry fst 'a' 'b', uncurry max (3, 8))
  print ("compose", compose3 show (+ 1) (* 2) 5)
  print ("dollar lambda", map (\f -> f 2) $ map add3 [1, 2] >>= \g -> [g 0])
  print ("constructor", Just (Just 3), (Just . Just) 3)
  print ("higher order", map (twice succ) "abc")
  print ("many args", foldr (\x k -> k . (x :)) id "abc" [])
  print ("section applied", (subtract 1) 10, (`div` 3) 10)
