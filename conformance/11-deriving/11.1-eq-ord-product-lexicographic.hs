-- Haskell 2010 Report 11.1: derived Eq and Ord for a single-constructor type compare the fields lexicographically, left to right
module Main (main) where

data P = P Int Char Bool deriving (Eq, Ord, Show)

data Date = Date Int Int Int deriving (Eq, Ord, Show)

-- fields of structured types
data Entry = Entry String [Int] (Maybe Char) (Int, Bool) deriving (Eq, Ord, Show)

data Unit = Unit deriving (Eq, Ord, Show)

data One = One Double deriving (Eq, Ord, Show)

main :: IO ()
main = do
  print ("equal", P 1 'a' True == P 1 'a' True, P 1 'a' True /= P 1 'a' True, compare (P 1 'a' True) (P 1 'a' True))
  print ("differ in 1st", P 1 'a' True == P 2 'a' True, compare (P 1 'z' True) (P 2 'a' False), P 2 'a' False > P 1 'z' True)
  print ("differ in 2nd", P 1 'a' True == P 1 'b' True, compare (P 1 'a' True) (P 1 'b' False), P 1 'b' False > P 1 'a' True)
  print ("differ in 3rd", P 1 'a' False == P 1 'a' True, compare (P 1 'a' False) (P 1 'a' True), P 1 'a' True > P 1 'a' False)
  print ("first difference decides", compare (P 0 'z' True) (P 1 'a' False), compare (P 1 'a' True) (P 0 'z' False), compare (P 1 'b' False) (P 1 'a' True))
  print ("all operators", P 1 'a' True < P 1 'b' True, P 1 'a' True <= P 1 'a' True, P 1 'b' True >= P 1 'a' True, P 1 'a' True > P 1 'a' True)
  print ("max/min", max (P 1 'a' True) (P 1 'a' False), min (P 3 'a' True) (P 2 'z' True))
  print ("dates", Date 2010 7 1 < Date 2010 12 1, Date 2010 12 31 < Date 2011 1 1, compare (Date 1998 2 1) (Date 1998 1 31))
  print ("dates max", maximum [Date 2010 7 1, Date 1998 12 31, Date 2010 6 30], minimum [Date 2010 7 1, Date 1998 12 31, Date 2010 6 30])
  print ("structured equal", Entry "a" [1, 2] (Just 'x') (0, True) == Entry "a" [1, 2] (Just 'x') (0, True))
  print ("structured: string field", compare (Entry "a" [9] (Just 'z') (9, True)) (Entry "b" [] Nothing (0, False)))
  print ("structured: list field", compare (Entry "a" [1, 2] Nothing (0, False)) (Entry "a" [1, 2, 0] Nothing (0, False)), compare (Entry "a" [2] Nothing (0, False)) (Entry "a" [1, 9] Nothing (0, False)))
  print ("structured: Maybe field", compare (Entry "a" [] Nothing (9, True)) (Entry "a" [] (Just 'a') (0, False)))
  print ("structured: tuple field", compare (Entry "a" [] Nothing (1, False)) (Entry "a" [] Nothing (1, True)), Entry "a" [] Nothing (1, True) /= Entry "a" [] Nothing (1, False))
  print ("no fields", Unit == Unit, Unit /= Unit, compare Unit Unit, Unit < Unit, Unit <= Unit, max Unit Unit)
  print ("one Double field", One 1.5 < One 2.5, One (-0.0) == One 0.0, One (0 / 0) == One (0 / 0), compare (One 2) (One 1))
  print ("same as tuple ordering", and [compare (P a b c) (P x y z) == compare (a, b, c) (x, y, z) | a <- [0, 1], b <- "ab", c <- [False, True], x <- [0, 1], y <- "ab", z <- [False, True]])
