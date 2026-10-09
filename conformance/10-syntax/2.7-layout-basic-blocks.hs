-- Haskell 2010 Report 2.7 / 10.3: basic layout for where, let, do, of: indentation opens, continues and closes blocks
module Main (main) where

f :: Int -> Int
f x = a + b
  where
    a = x * 2
    b = c + 1
      where c = x * 3

g :: Int -> Int
g x =
  let y = x + 1
      z = y * 2
   in y + z

h :: Maybe Int -> String
h m = case m of
  Nothing -> "none"
  Just n
    | n > 0 -> "pos"
    | otherwise -> "nonpos"

-- a continuation line indented more than the block is part of the same item
k :: Int
k = a +
      b +
    c
  where a = 1
        b = 2
        c = 3

-- where block starting on the same line as the keyword, items aligned with first token
m :: Int
m = p * q where p = 6
                q = 7

main :: IO ()
main = do
  print ("f", f 1)
  print ("g", g 1)
  print ("h", h Nothing, h (Just 1), h (Just 0))
  print ("k", k)
  print ("m", m)
  let a = 1
      b = 2
  print ("let in do", a + b)
  let c = 3
  let d = 4
  print ("two lets", c + d)
  let e = 5 in print ("let-in as statement", e)
  let { p = 1; q = 2 }
  print ("braced let in do", p + q)
  print
    ( "continued"
    , a
    , b
    )
  if a < b
    then print ("if in do", "then")
    else print ("if in do", "else")
  case a of
    1 -> print ("case in do", "one")
    _ -> print ("case in do", "other")
  print ("done", ())
