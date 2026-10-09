module Ops ((+++), minus, (===), Chain(..), Tree(..), Scale(..), plusR, (|>)) where

infixr 5 +++
infixl 6 `minus`
infix 4 ===
infixr 5 :>
infixl 6 :*:
infixl 7 .*.
infixr 6 `plusR`
infixr 9 |>

data Chain = Int :> Chain | End deriving Show

data Tree = Leaf Int | Tree :*: Tree deriving Show

class Scale a where
  (.*.) :: a -> a -> a

instance Scale Int where
  a .*. b = a * b

(+++) :: String -> String -> String
a +++ b = "(" ++ a ++ "+" ++ b ++ ")"

minus :: Int -> Int -> Int
minus = (-)

(===) :: Int -> Int -> Bool
(===) = (==)

plusR :: Int -> Int -> Int
plusR a b = a - b

(|>) :: Int -> Int -> Int
a |> b = a - b
