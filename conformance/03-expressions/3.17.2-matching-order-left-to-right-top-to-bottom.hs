-- Haskell 2010 Report 3.17.2: patterns are matched left to right, alternatives top to bottom; a failure stops before later bottoms
module Main (main) where

f :: (Int, Int) -> String
f (1, _) = "first is one"
f (_, 2) = "second is two"
f (_, _) = "neither"

g :: Bool -> Bool -> String
g False _ = "false first"
g _ False = "false second"
g True True = "both true"

args :: Int -> Int -> Int -> String
args 0 _ _ = "a"
args _ 0 _ = "b"
args _ _ 0 = "c"
args _ _ _ = "d"

overlap :: [Int] -> String
overlap (_ : _) = "general cons"
overlap [1] = "unreachable singleton"
overlap [] = "nil"

main :: IO ()
main = do
  print ("top to bottom", f (1, 2), f (3, 2), f (3, 4))
  -- the first component fails, so the second is never examined
  print ("fail before bottom in tuple", case (1, undefined) of { (2, 'a') -> "match"; _ -> "failed early" })
  print ("fail before bottom in list", case ['x', undefined] of { ['a', 'b'] -> "match"; _ -> "failed early" })
  print ("fail before bottom in cons", case 'x' : undefined of { ('a' : _ : _) -> "match"; _ -> "failed early" })
  print ("fail before bottom in constructor args", case Just (1, undefined) of { Just (0, True) -> "match"; _ -> "failed early" })
  -- arguments of a function are matched left to right
  print ("first clause decides", g False undefined)
  print ("left argument fails first", args 1 0 undefined, args 0 undefined undefined)
  print ("all clauses", args 0 1 1, args 1 0 1, args 1 1 0, args 1 1 1)
  print ("first matching clause wins", overlap [1], overlap [])
  -- wildcards and variables never force
  print ("wildcard in strict position", case (undefined, 1) of { (_, 1) -> "only second examined"; _ -> "other" })
  print ("variable in strict position", case (undefined, 1) of { (v, 1) -> "only second examined"; _ -> "other" })
  -- matching a constructor pattern forces only the outermost constructor
  print ("outermost only", case Just undefined of { Just _ -> "just"; Nothing -> "nothing" })
  print ("list spine only", case [undefined, undefined] of { [_, _] -> "two"; _ -> "other" })
  -- nested: subpatterns of one argument are finished before the next argument starts
  print ("depth first", case ((1, 2), undefined) of { ((1, 3), True) -> "match"; _ -> "failed in first component" })
  -- guards are tried only after the whole pattern has matched
  print ("guards after pattern", case (1, 2) of { (2, _) | undefined -> "a"; (1, n) | n > 1 -> "guard"; _ -> "none" })
  print ("as pattern order", case (1, undefined) of { whole@(2, _) -> fst whole; _ -> 0 })
