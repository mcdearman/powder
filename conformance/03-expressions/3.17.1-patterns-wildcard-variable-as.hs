-- Haskell 2010 Report 3.17.1: variable patterns, wildcards and as-patterns (including nested as-patterns)
module Main (main) where

third :: a -> b -> c -> c
third _ _ c = c

dupHead :: [a] -> [a]
dupHead whole@(x : _) = x : whole
dupHead [] = []

nestedAs :: [(Int, String)] -> String
nestedAs outer@(first@(n, name@(c : _)) : rest) =
  show (length outer) ++ " " ++ show first ++ " " ++ show n ++ " " ++ name ++ " " ++ [c] ++ " " ++ show (length rest)
nestedAs _ = "no match"

asOnVariable :: Int -> (Int, Int)
asOnVariable a@b = (a, b)

asOnWildcard :: Char -> Char
asOnWildcard c@_ = c

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes xs@(_ : rest) = xs : suffixes rest

main :: IO ()
main = do
  print ("variable", (\x -> x) 5)
  print ("wildcard", third 'a' "b" True)
  print ("wildcard on bottom", third (undefined :: Int) (undefined :: Char) "ok")
  print ("several wildcards", (\(_, _, z) -> z) (undefined :: Int, undefined :: Int, 'z'))
  print ("as", dupHead "abc", dupHead "")
  print ("nested as", nestedAs [(1, "one"), (2, "two")])
  print ("nested as fails inside", nestedAs [(1, "")], nestedAs [])
  print ("as on variable", asOnVariable 3)
  print ("as on wildcard", asOnWildcard 'w')
  print ("suffixes", suffixes "abc")
  print ("as in case", case Just [1, 2] of { m@(Just l@(h : _)) -> (m, l, h); _ -> (Nothing, [], 0) })
  print ("as in lambda", (\p@(a, b) -> (p, a + b)) (1, 2))
  print ("as in let", let t@(a, b) = (1, 'c') in (t, a, b))
  print ("as on literal", case 5 of { n@5 -> n + 1; _ -> 0 })
  print ("as on tuple of as", case (1, 2) of { p@(a@1, b@2) -> (p, a, b); _ -> ((0, 0), 0, 0) })
  print ("as value is the whole value", case [1, 2, 3] of { xs@(_ : ys@(_ : _)) -> (xs, ys); _ -> ([], []) })
  print ("underscore-prefixed variable", (\_unused x -> x) 'u' 'v')
  print ("primed variables", (\x x' x'' -> [x, x', x'']) 1 2 3)
