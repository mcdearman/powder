{-# LANGUAGE OverloadedStrings #-}
-- Text as bytes side by side, and maps and sets of Int that branch on
-- the bits of their keys: what the machines' bits and bytes are for.
import Data.Bits (popCount, shiftL, shiftR, xor, (.&.), (.|.))
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- How many times each word is in a text, by the word's length.
byLength :: T.Text -> IntMap.IntMap [T.Text]
byLength = foldl' (\m w -> IntMap.insertWith (++) (T.length w) [w] m) IntMap.empty . T.words

-- The numbers below a bound with no divisor but themselves and one.
primes :: Int -> IntSet.IntSet
primes n = foldl' strike (IntSet.fromList [2 .. n]) [2 .. n]
  where
    strike s p
      | p * p > n || IntSet.notMember p s = s
      | otherwise = foldl' (flip IntSet.delete) s [p * p, p * p + p .. n]

main :: IO ()
main = do
  let line = "the quick brown fox jumps over the lazy dog, na\239vely \8594 \128512"
  print (T.length line, T.toUpper (T.take 9 line), T.splitOn ", " line, T.isInfixOf "\8594" line)
  print (IntMap.toList (byLength line))
  print (T.replace "the" "a" line == T.intercalate "a" (T.splitOn "the" line), T.reverse (T.takeEnd 3 line), compare (T.take 3 line) "them")
  TIO.putStrLn (T.unwords (map (T.justifyRight 6 '.') (T.words (T.take 19 line))))
  let ps = primes 200
  print (IntSet.size ps, IntSet.toList (IntSet.filter (> 150) ps), IntSet.member 97 ps, IntSet.member 91 ps)
  let squares = IntMap.fromList [(k, k * k) | k <- [-6 .. 6]]
  print (IntMap.lookup (-4) squares, IntMap.keys (IntMap.filter even squares), IntMap.findMin squares, IntMap.foldrWithKey (\k v n -> k + v + n) 0 squares)
  print (IntSet.toList (IntSet.union (IntSet.fromList [-70, 0, 70]) (IntSet.map negate ps) `IntSet.intersection` IntSet.fromList [-80 .. 5]))
  print (5 .&. 3 :: Int, 5 .|. 3 :: Int, xor (-1) 255 :: Int, shiftL 1 40 :: Int, shiftR (-256) 4 :: Int, popCount (255 :: Int), shiftL 1 70 :: Integer, popCount (2 ^ 70 - 1 :: Integer))
