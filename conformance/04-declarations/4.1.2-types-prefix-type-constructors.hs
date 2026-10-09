-- Haskell 2010 Report 4.1.2: special type syntax in prefix form: (->) a b, [] a, (,) a b, (,,) a b c, ()
module Main (main) where

applyFn :: (->) Int Bool -> Int -> Bool
applyFn f x = f x

compose :: (->) b c -> (->) a b -> (->) a c
compose f g = \x -> f (g x)

listLen :: [] a -> Int
listLen = length

pairSwap :: (,) a b -> (,) b a
pairSwap (x, y) = (y, x)

triple :: (,,) Int Char Bool
triple = (1, 'x', True)

unitVal :: ()
unitVal = ()

unitFn :: () -> ()
unitFn () = ()

nested :: [] ((,) Int ([] Char))
nested = [(1, "one"), (2, "two")]

fnList :: [(->) Int Int]
fnList = [(+ 1), (* 2), subtract 3]

-- the function arrow associates to the right: Int -> (Int -> Int)
rightAssoc :: Int -> Int -> Int
rightAssoc x = \y -> x - y

explicitRight :: Int -> (Int -> Int)
explicitRight x y = x * y

higher :: (Int -> Int) -> Int
higher f = f 10

parenthesised :: ((Int)) -> ([(Bool)])
parenthesised n = replicate n True

main :: IO ()
main = do
  print ("applyFn", applyFn even 4, applyFn odd 4)
  print ("compose", compose show (+ (1 :: Int)) 41)
  print ("listLen", listLen "abcd", listLen ([] :: [Int]))
  print ("pairSwap", pairSwap (1 :: Int, 'a'))
  print ("triple", triple)
  print ("unit", unitVal, unitFn unitVal)
  print ("nested", nested)
  print ("fnList", map (\f -> f 10) fnList)
  print ("rightAssoc", rightAssoc 10 3, map (rightAssoc 1) [1, 2])
  print ("explicitRight", explicitRight 6 7)
  print ("higher", higher (+ 1), higher (rightAssoc 100))
  print ("parenthesised", parenthesised 2)
