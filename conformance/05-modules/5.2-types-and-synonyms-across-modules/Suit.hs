module Suit where

data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
