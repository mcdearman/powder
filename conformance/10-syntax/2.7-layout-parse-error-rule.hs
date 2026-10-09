-- Haskell 2010 Report 10.3: the parse-error(t) rule: an implicit block is closed where the next token would be illegal
module Main (main) where

f :: Int -> Int
f x = let y = x + 1 in y * 2

g :: Int -> (Int, Int)
g x = (let y = x in y, let z = x + 1 in z)

h :: Maybe Int -> Int
h m = (case m of Just n -> n; Nothing -> 0) + 1

k :: Int -> [Int]
k x = [let a = x in a, let b = x; c = b in c + 1, case x of 0 -> 0; _ -> 1]

w :: Int -> Int
w x = (y + z) where y = x; z = 2

lam :: Int -> Int
lam = \x -> let y = x in y + 1

inIf :: Bool -> Int
inIf b = if b then let t = 1 in t else let e = 2 in e

doOne :: Maybe Int
doOne = do x <- Just 1; y <- Just 2; return (x + y)

main :: IO ()
main = do
  print ("let in on one line", f 1)
  print ("let closed by comma and paren", g 1)
  print ("case closed by paren", h (Just 4), h Nothing)
  print ("blocks closed by comma and bracket", k 0, k 5)
  print ("where one line", w 3)
  print ("lambda let", lam 1)
  print ("let closed by else", inIf True, inIf False)
  print ("one-line do", doOne)
  print ("let in tuple", (let a = 1; b = 2 in a + b, 3))
  print ("nested one-line lets", let a = 1 in let b = a + 1 in let c = b + 1 in c)
  print ("let x = let", let a = let b = 2 in b + 1 in a * 2)
  print ("case in list", [case n of 1 -> "one"; _ -> "many" | n <- [1, 2 :: Int]])
  print ("do in parens", (do x <- [1, 2]; return (x * 2)) ++ [0])
  print ("case of case", case (case 1 of 1 -> 2; _ -> 3) of 2 -> "two"; _ -> "other")
  print ("let in right section", (+ let a = 1 in a) 2)
  print ("let then infix", (let a = 10 in a) + 1)
  print ("do closed by infix-free paren", length (do x <- "ab"; [x, x]))
  print ("let with guard-like rhs", let a | True = 1 | otherwise = 2 in a)
  print ("let closed by then", if let t = True in t then 1 else 2)
  print ("let closed by of", case let v = 3 in v of 3 -> "three"; _ -> "no")
