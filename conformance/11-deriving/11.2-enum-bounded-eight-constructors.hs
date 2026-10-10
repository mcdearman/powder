-- Haskell 2010 Report 11.2, 11.3: derived Enum and Bounded for an enumeration; derived Bounded for a type
-- with one constructor takes the bounds of its fields
module Main (main) where

data Note = A | B | C | D | E | F | G | H deriving (Show, Eq, Enum, Bounded)

data Both = Both Bool Ordering deriving (Show, Bounded)

main :: IO ()
main = do
  print (succ A, succ B, pred G, pred H)
  print [B ..]
  print [B, D .. G]
  print [D .. C]
  print [A, D ..]
  print [H, F ..]
  print [G, D .. A]
  print (minBound :: Note, maxBound :: Note)
  print (minBound :: Both, maxBound :: Both)
  print (map fromEnum [A, D, H], toEnum 5 :: Note)
  print ([minBound .. maxBound] == [A, B, C, D, E, F, G, H])
  print (length [A ..], take 3 (enumFromThen H G))
