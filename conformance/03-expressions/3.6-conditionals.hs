-- Haskell 2010 Report 3.6: if e1 then e2 else e3 = case e1 of { True -> e2; False -> e3 }
module Main (main) where

classify :: Int -> String
classify n = if n < 0 then "neg" else if n == 0 then "zero" else "pos"

viaCase :: Bool -> a -> a -> a
viaCase c t e = case c of { True -> t; False -> e }

main :: IO ()
main = do
  print ("true", if True then 1 else 2)
  print ("false", if False then 1 else 2)
  print ("chain", map classify [-1, 0, 1])
  print ("lazy then", if False then undefined else 'e')
  print ("lazy else", if True then 't' else undefined)
  print ("extends right", if True then 1 else 2 + 10)
  print ("parenthesised", (if True then 1 else 2) + 10)
  print ("as right operand", 100 + if False then 1 else 2)
  print ("as argument", succ (if True then 1 else 2))
  print ("condition expr", if 1 + 1 == 2 && not False then "y" else "n")
  print ("function result", (if True then (+) else (*)) 3 4, (if False then (+) else (*)) 3 4)
  print ("nested in then", if True then if False then 1 else 2 else 3)
  print ("nested in cond", if (if True then False else True) then 'a' else 'b')
  print ("overloaded", (if True then 1 else 2) :: Double)
  print ("in comprehension", [if even n then n else - n | n <- [1 .. 5]])
  print ("case translation", [(if c then 'T' else 'F') == viaCase c 'T' 'F' | c <- [False, True]])
  print ("with let", if let b = True in b then 1 else 0)
  print ("with lambda", (if True then \x -> x + 1 else \x -> x - 1) 10)
  if True then putStrLn "io then" else putStrLn "io else"
  if False
    then putStrLn "io then 2"
    else putStrLn "io else 2"
  r <- if 1 < (2 :: Int) then return 'y' else return 'n'
  print ("monadic result", r)
