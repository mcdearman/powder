-- Haskell 2010 Report 3.15, 4.2.1: a field label in several constructors of one type selects from and
-- updates each; a constructor declared with empty braces
module Main (main) where

data Uniform
  = A {a, b :: String}
  | B {a, b :: String}

data Semiuniform
  = C {c1, d :: String}
  | D {c2, d :: String}

data Disjoint
  = E {e, f, g :: String}
  | F {}

v1 :: Uniform
v1 = A {a = "a", b = "b"}

v2 :: Semiuniform
v2 = C {c1 = "c1", d = "d"}

v3 :: Disjoint
v3 = E {e = "e", f = "f", g = "g"}

v4 :: Uniform
v4 = v1 {b = "c"}

v5 :: Semiuniform
v5 = v2 {c1 = "c2"}

v6 :: Disjoint
v6 = v3 {g = "h"}

isF :: Disjoint -> Bool
isF F {} = True
isF _ = False

main :: IO ()
main = do
  putStrLn (b v1 ++ c1 v2 ++ d v2 ++ g v3)
  putStrLn (b v4 ++ c1 v5 ++ d v5 ++ g v6)
  putStrLn (a (B {a = "x", b = "y"}) ++ b ((B "p" "q") {a = "r"}) ++ d (D {c2 = "s", d = "t"}))
  putStrLn (d ((D "u" "v") {d = "w"}) ++ c2 ((D "u" "v") {d = "w"}) ++ e v6 ++ f v6)
  print (isF F, isF (F {}), isF v3)
