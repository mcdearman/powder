-- The library modules beside the prelude.
import Control.Monad (foldM, forM_, guard, unless, when, zipWithM_)
import Data.Char (isUpper, toUpper)
import Data.List (group, intercalate, isPrefixOf, nub, partition, sort, sortBy, transpose, (\\))
import qualified Data.List as L
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Ratio (denominator, numerator, (%))
import Numeric (showHex, showOct)

pythagorean :: Int -> [(Int, Int, Int)]
pythagorean n = do
  a <- [1 .. n]
  b <- [a .. n]
  c <- [b .. n]
  guard (a * a + b * b == c * c)
  return (a, b, c)

main :: IO ()
main = do
  print (sort [3, 1, 2], nub [1, 1, 2, 3, 2], [1, 2, 3, 4] \\ [2, 3])
  print (map (\g -> (head g, length g)) (group (sort "mississippi")))
  putStrLn (intercalate ", " (map (map toUpper) ["a", "bc"]))
  print (transpose ["abc", "de", "f"], partition isUpper "aBcD", "ab" `isPrefixOf` "abc")
  print (sortBy (\a b -> compare (snd b) (snd a)) (zip "abc" [2, 3, 1]), L.foldl' (+) 0 [1 .. 100])
  print (catMaybes [Just 1, Nothing, Just 3], fromMaybe 0 Nothing, mapMaybe (`lookup` [(1, 'a')]) [1, 2])
  print (pythagorean 13, sort (L.permutations [1, 2, 3]))
  let r = 3 % 4 + 1 % 6
  print (r, numerator r, denominator r)
  putStrLn (showHex 255 (' ' : showOct 8 ""))
  forM_ [1, 2, 3] (\i -> when (odd i) (print i))
  total <- foldM (\acc x -> do { unless (x < 3) (putStrLn "big"); return (acc + x) }) 0 [1, 2, 3]
  zipWithM_ (\i s -> putStrLn (show i ++ ": " ++ s)) [1 ..] ["one", "two"]
  print total
