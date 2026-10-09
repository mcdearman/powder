-- Haskell 2010 Report 3.17.2: the worked examples of the informal semantics (those that do not diverge)
module Main (main) where

main :: IO ()
main = do
  -- (\ ~(x,y) -> 0) _|_  =>  0
  print ("lazy pair", (\ ~(x, y) -> 0) (undefined :: (Int, Int)))
  -- (\ ~[x] -> 0) []  =>  0
  print ("lazy singleton vs nil", (\ ~[x] -> 0) ([] :: [Int]))
  -- (\ ~[x, ~(a,b)] -> x) [(0,1), _|_]  =>  (0,1)
  print ("nested lazy", (\ ~[x, ~(a, b)] -> x) [(0, 1), undefined])
  -- (\ (x:xs) -> x:x:xs) (0:_|_)  =>  0:0:_|_
  print ("cons with bottom tail", take 2 ((\(x : xs) -> x : x : xs) (0 : undefined)))
  -- matching ['a','b'] against ['x', _|_] fails, it does not diverge
  print ("list fails early", case ['x', undefined] of { ['a', 'b'] -> "matched"; _ -> "failed" })
  -- matching 1 against (1, _|_) style: (1, _) succeeds without touching the bottom
  print ("wildcard next to bottom", case (1, undefined) of { (1, _) -> "matched"; _ -> "failed" })
  -- a variable pattern binds without evaluating
  print ("variable binds bottom", (\x -> 1) (undefined :: Int))
  -- an irrefutable pattern binds its variables lazily
  print ("lazy binding used", (\ ~(x, y) -> x + y) (1, 2))
  -- data D = D Int with lazy field: D _|_ still matches D _
  print ("constructor with bottom field", case Just (undefined :: Int) of { Just _ -> "matched"; Nothing -> "failed" })
  -- literal: matching succeeds iff value == literal
  print ("literal", case 3 of { 3 -> "equal"; _ -> "not equal" }, case 4 of { 3 -> "equal"; _ -> "not equal" })
  -- as-pattern binds the whole value in addition to matching the subpattern
  print ("as", case [1, 2] of { xs@(x : _) -> (x, xs); _ -> (0, []) })
  -- string literal pattern against a list with a bottom tail fails at the first differing character
  print ("string fails early", case 'x' : undefined of { "abc" -> "matched"; _ -> "failed" })
