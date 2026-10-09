-- Haskell 2010 Report 10.6: an infixl and an infixr operator of the same precedence cannot be mixed without parentheses
module Main (main) where

infixl 5 <<<
infixr 5 >>>

(<<<) :: Int -> Int -> Int
a <<< b = a - b

(>>>) :: Int -> Int -> Int
a >>> b = a + b

main :: IO ()
main = print (1 <<< 2 >>> 3)
