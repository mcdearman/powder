-- Haskell 2010 Report 3.17.1: constructor, infix constructor, tuple and list patterns
module Main (main) where

infixr 5 :>

data Stream = End | Int :> Stream
  deriving (Eq, Show)

data Shape = Circle Int | Rect Int Int | Tri Int Int Int | Origin
  deriving (Eq, Show)

total :: Stream -> Int
total End = 0
total (n :> rest) = n + total rest

firstTwo :: Stream -> (Int, Int)
firstTwo (a :> b :> _) = (a, b)
firstTwo _ = (0, 0)

perimeter :: Shape -> Int
perimeter (Circle r) = 6 * r
perimeter (Rect w h) = 2 * (w + h)
perimeter (Tri a b c) = a + b + c
perimeter Origin = 0

len :: [a] -> Int
len [] = 0
len (_ : xs) = 1 + len xs

shape :: [Int] -> String
shape [] = "empty"
shape [_] = "one"
shape [_, _] = "two"
shape (_ : _ : _ : []) = "three"
shape (1 : 2 : _) = "starts 1 2"
shape _ = "long"

main :: IO ()
main = do
  print ("infix constructor", total (1 :> 2 :> 3 :> End), firstTwo (7 :> 8 :> End), firstTwo (7 :> End))
  print ("constructors", map perimeter [Circle 1, Rect 2 3, Tri 3 4 5, Origin])
  print ("cons", len "abcd", len ([] :: [Int]))
  print ("list shapes", map shape [[], [9], [9, 9], [9, 9, 9], [1, 2, 3, 4], [9, 9, 9, 9]])
  print ("tuple", (\(a, b) -> b - a) (1, 5), (\(a, b, c) -> [c, b, a]) ('x', 'y', 'z'))
  print ("tuple 7", (\(a, b, c, d, e, f, g) -> a + b + c + d + e + f + g) (1, 2, 3, 4, 5, 6, 7))
  print ("unit", (\() -> "unit") ())
  print ("Maybe", [case m of { Just n -> n; Nothing -> 0 } | m <- [Just 3, Nothing]])
  print ("Either", [case e of { Left n -> show n; Right s -> s } | e <- [Left 1, Right "r"]])
  print ("Bool", [case b of { True -> 1; False -> 0 } | b <- [True, False]])
  print ("Ordering", [case o of { LT -> -1; EQ -> 0; GT -> 1 } | o <- [LT, EQ, GT]])
  print ("backquoted constructor", case Rect 2 3 of { w `Rect` h -> w * h; _ -> 0 })
  print ("prefix cons", case "ab" of { (:) c _ -> c; [] -> '?' })
  print ("prefix infix constructor", case 1 :> End of { (:>) n _ -> n; End -> 0 })
  print ("parenthesised", case Just 1 of { ((Just (n))) -> n; (Nothing) -> 0 })
  print ("cons right assoc", case [1, 2, 3] of { a : b : c -> (a, b, c); _ -> (0, 0, []) })
  print ("list in tuple in list", case [([1, 2], 'a')] of { [(x : _, c)] -> (x, c); _ -> (0, '?') })
