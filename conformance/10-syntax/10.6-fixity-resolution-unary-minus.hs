-- Haskell 2010 Report 10.6 / 3.4: unary minus resolves as a prefix operator of precedence 6 (left-associative)
module Main (main) where

infixl 6 |+|
infixr 5 |:|
infix 4 |=|
infixl 7 |*|

(|+|) :: Int -> Int -> Int
a |+| b = a + b

(|*|) :: Int -> Int -> Int
a |*| b = a * b

(|:|) :: Int -> [Int] -> [Int]
a |:| b = a : b

(|=|) :: Int -> Int -> Bool
a |=| b = a == b

main :: IO ()
main = do
  print ("-a + b is (-a) + b", - 1 + 2, -1 + 2)
  print ("-a * b is -(a * b)", - 2 * 3, (-2) * 3)
  print ("-a ^ b is -(a ^ b)", - 2 ^ 2, (-2) ^ 2)
  print ("-a `mod` b is -(a `mod` b)", - 7 `mod` 3, (-7) `mod` 3)
  print ("-a `div` b is -(a `div` b)", - 7 `div` 2, (-7) `div` 2)
  print ("a - -b needs parens; a - (-b)", 5 - (-3))
  print ("-a - b is (-a) - b", - 1 - 2)
  print ("a + b * -c is illegal; with parens", 1 + 2 * (-3))
  print ("- a == b is (-a) == b", - 1 == -1, - 1 < 0)
  print ("a == -b is legal", 1 == -1, -1 == -1, 0 > -1)
  print ("-a : xs is (-a) : xs", - 1 : [2], -1 : -2 : [])
  print ("comparison rhs negation", 3 > - 4, 3 >= -3 + 6)
  print ("and with negation operand", True && -1 < 0)
  print ("user infixl 6", - 1 |+| 2, 1 |+| 2 - 3)
  print ("user infixl 7 under minus", - 2 |*| 3)
  print ("user infixr 5", - 1 |:| [], - 1 |:| - 2 |:| [])
  print ("user infix 4", - 1 |=| - 1)
  print ("negate in arguments", abs (-3), max (-1) (-2), (- 3) + 1)
  print ("negate applications", - abs 3, - succ 1 * 2, - fromIntegral (3 :: Int) + 0.5)
  print ("subtract function vs minus", subtract 1 5, (subtract 1) 5, (\x -> x - 1) 5)
  print ("minus section is negation", (- 1), (-1), (- 1) + 0)
  print ("double negation with parens", - (- 1), -(-(-1)))
  print ("negative float", - 1.5 * 2, -1.5e1 + 1)
  print ("negation of if/let via parens", - (if True then 1 else 2), - (let a = 3 in a))
  print ("e1 - e2 with spacing variants", 5-3, 5 -3, 5- 3, 5 - 3)
  let x = 4 :: Int
  print ("x-1 x -1 x - 1", x-1, x -1, x - 1)
  print ("negate variable", -x, - x, -x + 1, -x * 2)
