-- Haskell 2010 Report 3.15 / 3.17.1: patterns with field labels; C {} matches any constructor, record or not
module Main (main) where

data P = P { px :: Int, py :: Int, pname :: String }
  deriving (Eq, Show)

data Shape
  = Circle { label :: String, radius :: Int }
  | Rect { label :: String, width, height :: Int }
  deriving (Eq, Show)

data Plain = A Int Int | B | C Char
  deriving (Eq, Show)

-- record braces bind tighter than application, so no parentheses are needed
origin :: P -> Bool
origin P { px = 0, py = 0 } = True
origin _ = False

area :: Shape -> Int
area Circle { radius = r } = 3 * r * r
area Rect { height = h, width = w } = w * h

which :: Plain -> String
which A {} = "A"
which B {} = "B"
which C {} = "C"

nameOf :: P -> String
nameOf (P { pname = n }) = n

main :: IO ()
main = do
  let p = P 1 2 "pt"
  print ("literal fields", origin (P 0 0 "o"), origin p)
  print ("some fields, any order", area (Circle "c" 2), area (Rect "r" 3 4))
  print ("empty braces on non-record", map which [A 1 2, B, C 'x'])
  print ("empty braces on record", case p of { P {} -> "any P" })
  print ("empty braces do not force fields", which (A undefined undefined), case P undefined undefined undefined of { P {} -> "lazy" })
  print ("one field", nameOf p)
  print ("as pattern", case p of { whole@P { px = x } -> (x, whole) })
  print ("nested pattern in field", case Circle "c" 7 of { Circle { label = (ch : _), radius = 7 } -> [ch]; _ -> "no" })
  print ("mixed with positional", case p of { P 1 y _ -> y; P { py = y } -> y + 100 })
  print ("lambda", (\P { px = x, py = y } -> x + y) p)
  print ("let", let P { pname = n, px = x } = p in (n, x))
  print ("comprehension", [x | P { px = x, py = 2 } <- [p, P 5 2 "a", P 6 3 "b"]])
  print ("unmentioned fields not forced", origin (P 0 0 undefined), nameOf (P undefined undefined "n"))
  -- fields are matched in the order they are written in the pattern, not in declaration order
  print ("fails on first mentioned field", case P undefined 9 "f" of { P { py = 0, px = 0 } -> "match"; _ -> "no match" })
  print ("wildcard field", case p of { P { px = _, pname = n } -> n })
  print ("prelude constructors", [case m of { Just {} -> "just"; Nothing {} -> "nothing" } | m <- [Just 'x', Nothing]])
