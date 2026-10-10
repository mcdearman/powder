-- Haskell 2010 Report 3.3: a lambda's parameters are patterns, which may be ones that can fail to match
module Main (main) where

match :: [Maybe ()] -> ()
match = \[Nothing, Just ()] -> ()

identity :: a -> a
identity = \x -> x

main :: IO ()
main = do
  print (match [Nothing, Just ()])
  print (identity 'i', identity (identity True))
  print ((\(x : _) (_, y) -> (x, y)) "ab" (1 :: Int, 'c'))
  print (map (\(Just n) -> n + 1) [Just 1, Just (2 :: Int)])
  print ((\1 'a' "s" -> "literals") (1 :: Int) 'a' "s")
  print ((\whole@(h : _) ~(_ : rest) -> (whole, h, rest)) "xy" "pq")
  print ((\_ _ -> "wildcards") (undefined :: Int) (undefined :: Bool))
