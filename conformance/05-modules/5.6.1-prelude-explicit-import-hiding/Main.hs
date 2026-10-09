-- Haskell 2010 Report 5.6: an explicit `import Prelude` replaces the implicit one; hidden Prelude names may be redefined
module Main (main) where

import Prelude hiding (lookup, filter, (++), Maybe(..), length, id)
import qualified Prelude as P

data Maybe a = Nothing | Just a | Perhaps a deriving Show

infixr 5 ++

(++) :: [a] -> [a] -> [a]
xs ++ ys = P.foldr (:) ys xs

filter :: Int -> Int
filter = (+ 1)

lookup :: String
lookup = "local lookup"

length :: [a] -> Integer
length = P.foldr (\_ n -> n + 1) 0

id :: Int
id = 7

main :: IO ()
main = do
  print ("local Maybe", [Nothing, Just 1, Perhaps (2 :: Int)])
  print ("Prelude Maybe qualified", P.Just 'x', (P.Nothing :: P.Maybe Int))
  print ("local append", [1, 2] ++ [3 :: Int], "ab" ++ "cd" ++ "ef")
  print ("local filter/lookup/id", filter 1, lookup, id)
  print ("local length is Integer", length "abc" + 2 ^ 70)
  print ("Prelude versions qualified", P.filter even [1, 2, 3, 4 :: Int], P.lookup 1 [(1 :: Int, 'a')])
  print ("Prelude length and id", P.length "abc", P.id 'c')
  print ("unhidden names still unqualified", map negate [1, 2 :: Int], show 3)
