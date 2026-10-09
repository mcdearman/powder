-- Haskell 2010 Report 4.4.2 / 5.2: fixity is a property of the entity and follows it through exports, re-exports and qualification
module Main (main) where

import Ops
import qualified Ops as O
import ReOps (plusR, (|>))

infixl 1 |>>

-- a local operator with its own fixity next to imported ones
(|>>) :: Int -> Int -> Int
a |>> b = a * 100 + b

main :: IO ()
main = do
  print ("infixr 5 from Ops", "1" +++ "2" +++ "3")
  print ("infixl 6 backquoted function from Ops", 10 `minus` 3 `minus` 2)
  print ("infix 4 with tighter arithmetic", 1 + 1 === 2, show (2 * 2 === 3 + 1))
  print ("constructor operator infixr 5", 1 :> 2 :> End)
  print ("derived Show uses the imported fixity", Leaf 1 :*: Leaf 2 :*: Leaf 3)
  print ("class method operator infixl 7", 2 + 3 .*. (4 :: Int), (2 + 3) .*. (4 :: Int))
  print ("qualified use keeps fixity", "1" O.+++ "2" O.+++ "3", 10 `O.minus` 3 `O.minus` 2)
  print ("re-exported with a different route", 1 `plusR` 2 `plusR` 3, 1 |> 2 |> 3)
  print ("mixing imported and local fixities", 10 `minus` 3 |>> 2 `minus` 1)
  print ("sections", (+++ "1") "2", ("1" +++) "2", (`minus` 1) 5)
