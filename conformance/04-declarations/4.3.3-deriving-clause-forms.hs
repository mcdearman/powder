-- Haskell 2010 Report 4.3.3: forms of the deriving clause: one class, a parenthesised list, the empty list, none
module Main (main) where

data A = A1 | A2 deriving Eq

data B = B1 | B2 deriving (Eq, Ord)

data C = C1 | C2 deriving ()

data D = D1 Int | D2 deriving (Show)

data E = E1 | E2 | E3
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

newtype F = F Int deriving (Eq, Ord, Show)

data G a = G a a deriving (Prelude.Eq, Prelude.Show)

data H = H1 | H2

newtype I = I Bool deriving Show

cName :: C -> String
cName C1 = "c1"
cName C2 = "c2"

hName :: H -> String
hName H1 = "h1"
hName H2 = "h2"

main :: IO ()
main = do
  print ("deriving Eq", A1 == A1, A1 == A2, A1 /= A2)
  print ("deriving (Eq, Ord)", B1 < B2, compare B2 B1, B1 == B1)
  print ("deriving ()", cName C1, cName C2)
  print ("deriving (Show)", D1 5, D2)
  print ("six classes show", [E1, E2, E3])
  print ("six classes eq ord", E1 == E1, E1 < E3, maximum [E2, E3, E1])
  print ("six classes read", read "E2" :: E)
  print ("six classes enum", [E1 ..], succ E1, fromEnum E3)
  print ("six classes bounded", minBound :: E, maxBound :: E)
  print ("newtype", F 1, F 1 == F 1, F 1 < F 2)
  print ("qualified class names", G 'a' 'b', G True True == G True True)
  print ("no deriving clause", hName H1, hName H2)
  print ("newtype single class", I True)
