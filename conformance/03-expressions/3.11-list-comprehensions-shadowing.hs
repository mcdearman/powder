-- Haskell 2010 Report 3.11: scoping in list comprehensions; later qualifiers and the result see earlier bindings, which may be shadowed
module Main (main) where

main :: IO ()
main = do
  let x = 100 :: Int
      xs = [1, 2] :: [Int]
  -- the generator binds a new x; its source expression still sees the outer or earlier x
  print ("shadow outer", [x | x <- [1, 2, 3]])
  print ("outer unchanged", x)
  print ("source sees outer", [x | x <- [x, x + 1]])
  print ("second generator shadows first", [x | x <- [1, 2], x <- [x * 10, x * 20]])
  print ("let shadows generator", [x | x <- [1, 2], let x = 'c'])
  print ("generator not recursive", [xs | xs <- [xs, xs]])
  print ("let then generator", [y | y <- [1, 2, 3], let y' = y * 2, y <- [y', y' + 1]])
  print ("inner comprehension shadows", [[x | x <- [x .. 3]] | x <- [1, 2, 3]])
  print ("guard sees innermost", [x | x <- [1 .. 3], x <- "abc", x /= 'b'])
  print ("result sees all", [(a, b, c) | a <- [1, 2], let b = a * 2, c <- [b, b + 1]])
  print ("pattern vars shadow", [x | (x, _) <- [(1, 'a'), (2, 'b')]])
  print ("outer still unchanged", x, xs)
  print ("function name shadowed", [succ | succ <- [1, 2, 3]])
