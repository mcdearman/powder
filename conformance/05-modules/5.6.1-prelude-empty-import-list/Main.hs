-- Haskell 2010 Report 5.6.1: `import Prelude ()` brings no Prelude names into scope, but built-in syntax (lists, tuples, unit,
-- literals, [a..b], if, do) still refers to the Prelude entities; `import qualified Prelude` gives access by qualified name
module Main (main) where

import Prelude ()
import qualified Prelude

data Bool = No | Yes

show :: Bool -> Prelude.String
show No = "No"
show Yes = "Yes"

not :: Bool -> Bool
not No = Yes
not Yes = No

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

main :: Prelude.IO ()
main = do
  Prelude.print ("local show", show Yes, show (not Yes))
  Prelude.print ("list and tuple syntax", [1, 2, 3 :: Prelude.Int], (1 :: Prelude.Int, 'c', ()))
  Prelude.print ("literals", 42 :: Prelude.Integer, 1.5 :: Prelude.Double, "str")
  Prelude.print ("enumFromTo sugar", [1 .. 3 :: Prelude.Int])
  Prelude.print ("if uses Prelude Bool", if 1 Prelude.< (2 :: Prelude.Int) then 'y' else 'n')
  Prelude.print ("local map", map show [No, Yes])
  Prelude.print ("negation sugar", - (5 :: Prelude.Int))
  Prelude.print ("comprehension", [x | x <- "abc", x Prelude./= 'b'])
