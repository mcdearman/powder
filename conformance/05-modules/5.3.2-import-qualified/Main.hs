-- Haskell 2010 Report 5.3.2: `import qualified` brings only qualified names into scope; qualified import lists and hiding
module Main (main) where

import qualified Zoo
import qualified Data.List (sort, nub)
import qualified Data.Char hiding (toUpper)

-- unqualified names are not in scope, so all of these are fine
data Animal = Cat | Rat deriving Show

legs :: Animal -> Int
legs _ = 3

sort :: String
sort = "local sort"

toUpper :: Char -> Char
toUpper = id

(<+>) :: Int -> Int -> Int
a <+> b = a * b

main :: IO ()
main = do
  print ("qualified constructors", [Zoo.Cat, Zoo.Dog, Zoo.Emu])
  print ("local vs qualified", Cat, Zoo.Cat, legs Cat, Zoo.legs Zoo.Cat)
  print ("qualified function", Zoo.feed Zoo.Emu)
  print ("qualified operator", 1 Zoo.<+> 2, (Zoo.<+>) 3 4, 3 <+> 4)
  print ("qualified operator keeps its fixity", 2 * 3 Zoo.<+> 4 * 5)
  print ("qualified method", Zoo.noise Zoo.Dog, Zoo.loud Zoo.Cat)
  print ("qualified field select", Zoo.number (Zoo.Cage Zoo.Cat 9))
  print ("qualified field in construction", Zoo.Cage { Zoo.occupant = Zoo.Dog, Zoo.number = 1 })
  print ("qualified field in update", (Zoo.Cage Zoo.Cat 1) { Zoo.number = 2 })
  print ("qualified pattern", case Zoo.Cage Zoo.Emu 4 of Zoo.Cage { Zoo.number = n } -> n)
  print ("qualified type in signature", (Zoo.Cat :: Zoo.Animal), length ([] :: Zoo.Roster))
  print ("qualified with import list", Data.List.sort [3, 1, 2 :: Int], Data.List.nub "aab", sort)
  print ("qualified with hiding", Data.Char.toLower 'A', Data.Char.ord 'a', toUpper 'a')
  print ("qualified backquotes", Zoo.Cat `Zoo.sameLegs` Zoo.Dog, 3 `Prelude.max` 4)
