-- Haskell 2010 Report 10.3: explicit-brace blocks nested inside layout blocks and vice versa; semicolons in layout blocks
module Main (main) where

f :: Int -> Int
f x = case x of { 0 -> a; _ -> b }
  where
    a = 10; b = 20

g :: Int -> Int
g x = let { y = x + 1 } in
  case y of
    1 -> 100
    _ -> let { z = y
             ; w = z } in w

-- an implicit block nested inside an explicit one
h :: Int -> Int
h x = r where { r = case x of
                      0 -> 1
                      _ -> 2 }

-- several declarations on one line of a layout block, separated by semicolons
p, q, r3 :: Int
p = 1; q = 2; r3 = 3

main :: IO ()
main = do
  print ("f", f 0, f 1)
  print ("g", g 0, g 5)
  print ("h", h 0, h 9)
  print ("pqr", p, q, r3)
  let a = 1; b = 2
      c = 3
  print ("semicolons in implicit let", a + b + c)
  print ("stmt1", 1); print ("stmt2", 2)
  do { print ("explicit inner do", 1)
     ; print ("explicit inner do", 2)
     }
  mapM_ (\n -> do
            print ("lambda do", n)
            print ("lambda do again", n))
        [1, 2 :: Int]
  r <- do
    let t = 5
    return (t * 2)
  print ("nested do result", r)
  case r of
    10 -> do print ("case-do", "a")
             print ("case-do", "b")
    _ -> return ()
  where_test
  where
    where_test = do { print ("where after do", 1) ; print ("where after do", 2) }
