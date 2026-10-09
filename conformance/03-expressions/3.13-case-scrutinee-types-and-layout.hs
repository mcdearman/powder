-- Haskell 2010 Report 3.13: case over many scrutinee types; evaluation of the scrutinee; empty alternatives and explicit braces
module Main (main) where

data Color = Red | Green | Blue
  deriving (Eq, Show)

name :: Color -> String
name c = case c of
  Red -> "red"
  Green -> "green"
  Blue -> "blue"

braces :: Int -> String
braces n = case n of { ; 1 -> "one" ; ; 2 -> "two" ; _ -> "many" ; }

onOneLine :: Bool -> Int
onOneLine b = case b of True -> 1; False -> 0

main :: IO ()
main = do
  print ("enumeration", map name [Red, Green, Blue])
  print ("bool", case 1 < 2 of { True -> "t"; False -> "f" })
  print ("char", [case c of { 'a' -> 1; '\n' -> 2; _ -> 3 } | c <- "a\nz"])
  print ("int", [case n of { 0 -> "z"; -1 -> "m"; _ -> "o" } | n <- [0, -1, 5]])
  print ("double", [case d of { 0.5 -> "half"; 2 -> "two"; _ -> "o" } | d <- [0.5, 2.0, 3.0]])
  print ("unit", case () of { () -> "unit" })
  print ("tuple", case (1, 'a', True) of { (n, c, True) -> (c, n); (_, c, False) -> (c, 0) })
  print ("ordering", [case compare a 2 of { LT -> '<'; EQ -> '='; GT -> '>' } | a <- [1, 2, 3]])
  print ("function result", case lookup 2 [(1, "one"), (2, "two")] of { Just s -> s; Nothing -> "none" })
  print ("extra semicolons", map braces [1, 2, 3])
  print ("one line layout", onOneLine True, onOneLine False)
  -- a variable or wildcard alternative does not evaluate the scrutinee
  print ("wildcard on bottom", case (undefined :: Int) of { _ -> "not forced" })
  print ("variable on bottom", case (undefined :: Int) of { v -> "not forced" })
  print ("lazy pattern on bottom", case (undefined :: (Int, Int)) of { ~(a, b) -> "not forced" })
  -- a constructor pattern evaluates only to weak head normal form
  print ("whnf only", case (undefined, undefined) :: (Int, Int) of { (_, _) -> "pair" })
  print ("whnf cons", case (undefined : undefined) :: [Int] of { (_ : _) -> "cons"; [] -> "nil" })
  print ("case of case", case (case 3 of { 3 -> Just 'x'; _ -> Nothing }) of { Just c -> c; Nothing -> '-' })
  print ("alternative body extends right", case 2 of { n -> n + 1 * 3 })
