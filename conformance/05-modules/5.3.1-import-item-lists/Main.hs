-- Haskell 2010 Report 5.3.1: import lists: var, T, T(..), T(C1,C2), T(field), class C(..), C(m); only listed names enter scope
module Main (main) where

import Zoo (Animal(Cat, Dog), Cage(..), Keeper, Roster, Noisy(noise), legs, (<+>))

-- not imported, so these definitions do not clash with Zoo's
data Bird = Emu deriving Show

feed :: Int -> Int
feed = (+ 1)

loud :: String
loud = "local loud"

zooName :: Int
zooName = 0

roster :: Roster
roster = []

keeperCount :: [Keeper] -> Int
keeperCount = length

main :: IO ()
main = do
  print ("listed constructors", [Cat, Dog])
  print ("unlisted constructor is free", Emu)
  print ("T(..) constructors and fields", Cage Cat 1, occupant (Cage Dog 2), number (Cage Dog 2))
  print ("record syntax", Cage { occupant = Cat, number = 3 })
  print ("type-only import usable in signatures", length roster, keeperCount [])
  print ("class method listed", noise Cat, noise Dog)
  print ("function and operator", legs Cat, 1 <+> 2 <+> 3)
  print ("fixity travels with the operator", 2 * 3 <+> 4 * 5)
  print ("local names", feed 1, loud, zooName)
  print ("instances come along regardless", Cat == Cat, Cat < Dog, [minBound .. Dog])
