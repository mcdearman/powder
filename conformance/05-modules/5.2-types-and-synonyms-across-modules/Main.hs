-- Haskell 2010 Report 5.2 / 4.2: data, newtype and synonym declarations used across modules; derived instances
-- (Eq, Ord, Show, Read, Enum, Bounded) of imported types; a synonym expanding to a type from a third module
module Main (main) where

import Data.List (sort)
import Suit
import Card (Card(..), Deck, Hand, fullDeck, mkHand, Rank)

best :: Hand -> Card
best = maximum

ranks :: Deck -> [Rank]
ranks = map rank

main :: IO ()
main = do
  print ("derived Enum/Bounded of imported type", [minBound .. maxBound :: Suit])
  print ("derived Ord", sort [Spades, Hearts, Clubs], Hearts < Spades)
  print ("derived Read", (read "Diamonds" :: Suit), (read "[Clubs,Hearts]" :: [Suit]))
  print ("record from other module", Card { rank = 3, suit = Hearts })
  print ("read record", (read "Card {rank = 9, suit = Spades}" :: Card))
  print ("synonyms", length fullDeck, take 2 fullDeck)
  print ("synonym of synonym", best (mkHand [Card 2 Clubs, Card 9 Hearts, Card 9 Clubs]))
  print ("synonym in local signature", take 3 (ranks fullDeck))
  print ("show . read", (read (show (Card 5 Diamonds)) :: Card) == Card 5 Diamonds)
