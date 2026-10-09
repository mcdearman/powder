-- Haskell 2010 Report 4.3.2: user instances of the Prelude classes Eq and Ord
module Main (main) where

import Data.Char (toLower)

data Suit = Clubs | Hearts | Spades

data Pair a = Pair a a

newtype CI = CI String

-- methods defined by infix operator clauses
instance Eq Suit where
  Clubs == Clubs = True
  Hearts == Hearts = True
  Spades == Spades = True
  _ == _ = False

instance Ord Suit where
  compare a b = compare (rank a) (rank b)
    where
      rank :: Suit -> Int
      rank Clubs = 0
      rank Hearts = 1
      rank Spades = 2

instance Eq a => Eq (Pair a) where
  Pair a b == Pair c d = a == c && b == d

-- ordered by second component first
instance Ord a => Ord (Pair a) where
  Pair a b <= Pair c d = b < d || (b == d && a <= c)

-- case-insensitive equality and ordering
instance Eq CI where
  CI a == CI b = map toLower a == map toLower b

instance Ord CI where
  compare (CI a) (CI b) = compare (map toLower a) (map toLower b)

suitName :: Suit -> String
suitName Clubs = "clubs"
suitName Hearts = "hearts"
suitName Spades = "spades"

unCI :: CI -> String
unCI (CI s) = s

main :: IO ()
main = do
  print ("suit ==", Clubs == Clubs, Clubs == Spades)
  print ("suit /=", Clubs /= Clubs, Hearts /= Spades)
  print ("suit compare", compare Clubs Hearts, compare Spades Hearts, compare Hearts Hearts)
  print ("suit operators", Clubs < Hearts, Clubs >= Hearts, Spades > Clubs, Hearts <= Hearts)
  print ("suit max min", suitName (max Clubs Spades), suitName (min Hearts Spades))
  print ("suit maximum", suitName (maximum [Hearts, Clubs, Spades, Hearts]))
  print ("suit in list", [Clubs, Hearts] == [Clubs, Hearts], [Clubs] < [Clubs, Hearts])
  print ("suit in maybe", Just Clubs == Just Clubs, Nothing < Just Clubs)
  print ("suit in tuple", (Clubs, 'a') < (Clubs, 'b'), (Spades, 'a') < (Clubs, 'b'))
  print ("pair ==", Pair 1 2 == Pair (1 :: Int) 2, Pair 'a' 'b' /= Pair 'a' 'b')
  print ("pair <=", Pair 9 1 <= Pair (0 :: Int) 2, Pair 9 2 <= Pair (0 :: Int) 2)
  print ("pair compare", compare (Pair 9 1) (Pair (0 :: Int) 2), compare (Pair 'x' 'y') (Pair 'x' 'y'))
  print ("pair of suits", Pair Clubs Spades > Pair Spades Clubs)
  print ("ci ==", CI "Hello" == CI "hELLO", CI "a" == CI "b")
  print ("ci compare", compare (CI "apple") (CI "BANANA"), CI "B" < CI "a")
  print ("ci max", unCI (max (CI "Zeta") (CI "alpha")))
