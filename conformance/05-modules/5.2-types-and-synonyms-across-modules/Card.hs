module Card (Card(..), Deck, Hand, Rank, fullDeck, mkHand) where

import Suit

type Rank = Int

data Card = Card { rank :: Rank, suit :: Suit }
  deriving (Eq, Ord, Show, Read)

type Deck = [Card]

type Hand = Deck

fullDeck :: Deck
fullDeck = [Card r s | s <- [minBound .. maxBound], r <- [1 .. 3]]

mkHand :: [Card] -> Hand
mkHand = id
