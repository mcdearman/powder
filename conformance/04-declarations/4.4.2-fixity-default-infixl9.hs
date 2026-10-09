-- Haskell 2010 Report 4.4.2: an operator with no fixity declaration is infixl 9
module Main (main) where

(<->) :: Int -> Int -> Int
a <-> b = a - b

minus :: Int -> Int -> Int
minus a b = a - b

data E = L Int | E :-: E | E `Sub` E

evalE :: E -> Int
evalE (L n) = n
evalE (a :-: b) = evalE a - evalE b
evalE (a `Sub` b) = evalE a - evalE b

main :: IO ()
main = do
  print ("operator left assoc", 10 <-> 3 <-> 2)
  print ("operator tighter than *", 2 * 3 <-> 1)
  print ("operator tighter than ^", 2 ^ 3 <-> 1)
  print ("operator tighter than +", 1 + 10 <-> 4 + 1)
  print ("backquoted left assoc", 10 `minus` 3 `minus` 2)
  print ("backquoted tighter than *", 2 * 10 `minus` 4)
  print ("backquoted tighter than ^", 2 ^ 3 `minus` 1)
  print ("same level as !!", [10, 20, 30] !! 1 <-> 5)
  print ("operator and backquoted", 10 <-> 3 `minus` 2)
  print ("looser than application", succ 1 <-> pred 5)
  print ("negation is level 6", - 2 <-> 1)
  print ("constructor operator", evalE (L 10 :-: L 3 :-: L 2))
  print ("backquoted constructor", evalE (L 10 `Sub` L 3 `Sub` L 2))
  print ("constructors mixed", evalE (L 10 :-: L 3 `Sub` L 2 :-: L 1))
  print ("comparison is looser", 5 <-> 2 == 3, 5 `minus` 2 < 2 * 2)
