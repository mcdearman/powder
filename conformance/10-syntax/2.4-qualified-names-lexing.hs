-- Haskell 2010 Report 2.4: qualified names are single lexemes; f.g (composition) vs F.g (qualified) vs F . g
module Main (main) where

import qualified Prelude
import Prelude hiding (lookup)
import qualified Data.Char
import qualified Data.List as L
import qualified Data.Maybe as Data.Maybe.Alias

data F = F Int deriving Show

f :: Int -> Int
f = (+ 1)

g :: Int -> Int
g = (* 2)

lookup :: Int
lookup = 99

main :: IO ()
main = do
  print ("f.g", (f.g) 5)
  print ("f . g", (f . g) 5)
  print ("constructor composed", (F . g) 5)
  print ("constructor composed tight right", (F .g) 5)
  print ("qualified var", Prelude.length "abc", Data.Char.toUpper 'a')
  print ("qualified op", 1 Prelude.+ 2, 7 Prelude.- 2, 3 Prelude.* 4)
  print ("qualified op prefix", (Prelude.+) 1 2, (Prelude..) f g 5)
  print ("qualified composition op", (f Prelude.. g) 5)
  print ("qualified backquote", 7 `Prelude.div` 2, 2 `Prelude.elem` [1, 2, 3])
  print ("qualified constructor", Prelude.Just 1, Prelude.True, Prelude.LT)
  print ("alias", L.sort [3, 1, 2], L.nub [1, 1, 2])
  print ("dotted alias", Data.Maybe.Alias.fromMaybe 0 (Just 4))
  print ("local vs qualified", lookup, Prelude.lookup 1 [(1, "one")])
  print ("enum from qualified constructor", [Prelude.False ..])
  print ("qualified in section", (Prelude.subtract 1) 10, (`Prelude.mod` 3) 10)
  print ("qualified type", (3 :: Prelude.Int), ("s" :: Prelude.String))
