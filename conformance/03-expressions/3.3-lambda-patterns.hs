-- Haskell 2010 Report 3.3: lambda abstractions with several pattern arguments
module Main (main) where

main :: IO ()
main = do
  print ("one arg", (\x -> x + 1) 2)
  print ("two args", (\x y -> x - y) 5 3)
  print ("three args", (\x y z -> [x, y, z]) 1 2 3)
  print ("tuple", (\(a, b) -> a + b) (1, 2))
  print ("cons", (\(x : _) -> x) [9, 8])
  print ("wildcard", (\_ -> 'k') (undefined :: Int))
  print ("wildcards", (\_ _ c -> c) 'a' 'b' 'c')
  print ("lazy", (\ ~(_, _) c -> c) (undefined :: (Int, Int)) 5)
  print ("mixed", (\(Just x) [y] -> x + y) (Just 1) [2])
  print ("as", (\x@(a, _) -> (x, a)) (1, 'c'))
  print ("literal", (\1 -> "one") 1)
  print ("string", (\"hi" -> True) "hi")
  print ("nested lambda", (\x -> \y -> x * y) 3 4)
  print ("body extends right", (\x -> x + 1 * 2) 3)
  print ("map", map (\(a, b) -> a * b) [(1, 2), (3, 4)])
  print ("shadow", (\x -> (\x -> x * 10) (x + 1)) 1)
  print ("partial", let f = \x y -> x * y in map (f 3) [1, 2])
  print ("returns lambda", ((\x -> (\y -> x + y)) 1) 2)
  print ("lists and tuples", (\[a, b] (c, d) -> a + b + c + d) [1, 2] (3, 4))
  print ("unit", (\() -> "unit") ())
  -- \p1 p2 -> e  =  \x1 x2 -> case (x1, x2) of (p1, p2) -> e
  print ("translation", (\(a, b) [c] -> a + b + c) (1, 2) [3]
                        == (\x1 x2 -> case (x1, x2) of { ((a, b), [c]) -> a + b + c }) (1, 2) [3])
  print ("arg to function", foldr (\x acc -> acc ++ [x]) [] "abc")
  print ("as right operand", (id $ \x -> x + 1) 4)
