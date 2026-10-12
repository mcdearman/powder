{-# LANGUAGE ViewPatterns #-}
-- ViewPatterns: (function -> pattern) matches what the pattern does, of the function applied to what is matched
module Main (main) where

import Data.Char (toLower, isDigit)
import Data.List (stripPrefix)

data Shape = Circle Int | Box Int Int

area :: Shape -> Int
area (Circle r) = 3 * r * r
area (Box w h) = w * h

size :: Shape -> String
size (area -> 0) = "nothing"
size (area -> a) | a < 20 = "small"
size (area -> a) = "big: " ++ show a

command :: String -> String
command (map toLower -> "quit") = "leaving"
command (stripPrefix "say " -> Just rest) = rest
command (words -> [a, b]) = b ++ " " ++ a
command (span isDigit -> (digits@(_ : _), "")) = "number " ++ digits
command other = "unknown: " ++ other

-- Inside other patterns, and several in one.
both :: (Int, [Int]) -> Maybe Int -> String
both (even -> True, length -> 2) (fmap (+ 1) -> Just 3) = "all three"
both (even -> True, _) _ = "even"
both (_, sum -> s) _ = "sum " ++ show s

-- One inside the pattern of another.
nested :: String -> String
nested (words -> (reverse -> c : _) : _) = [c]
nested _ = "none"

-- The function may use what is bound before it.
within :: Int -> Int -> Bool
within limit ((<= limit) -> ok) = ok

lookupBoth :: [(Int, String)] -> Int -> String
lookupBoth table (flip lookup table -> Just v) = v
lookupBoth _ _ = "missing"

-- What a where defines may use what a view pattern binds.
described :: [Int] -> String
described (length -> n) = text
  where
    text = show n ++ " long"

main :: IO ()
main = do
  print (map size [Circle 0, Circle 2, Box 5 5])
  print (map command ["QUIT", "say hello there", "a b", "1234", "what is this"])
  print [both (2, [1, 2]) (Just 2), both (2, [1]) Nothing, both (1, [1, 2, 3]) Nothing]
  print (map nested ["hello world", ""])
  print (within 3 2, within 3 4)
  print (lookupBoth [(1, "one")] 1, lookupBoth [(1, "one")] 2)
  print (described [1, 2, 3])
  print ((\(negate -> x) -> x) (5 :: Int))
  print (case "abc" of (length -> 3) -> "three"; _ -> "other")
  print [y | (lookup 'a' -> Just y) <- [[('a', 1 :: Int)], [('b', 2)], [('a', 3)]]]
  r <- (\m -> do { (reverse -> x : _) <- m; return x }) (return "xyz")
  print r
  print (do { (reverse -> x : _) <- Just ""; return (x :: Char) })
