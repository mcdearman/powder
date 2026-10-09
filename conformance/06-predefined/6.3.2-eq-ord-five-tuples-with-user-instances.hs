-- Haskell 2010 Report 6.1.4, 6.3.1, 6.3.2: Eq and Ord for a five-tuple whose components include a type with
-- hand-written instances; comparison is lexicographic
module Main (main) where

number :: Integer
number = 1

data Custom = Custom

instance Eq Custom where
  Custom == Custom = True

instance Ord Custom where
  compare Custom Custom = EQ

main :: IO ()
main = do
  print ("same", (number, True, Custom, "abc", EQ) == (1, True, Custom, "abc", EQ))
  print ("less in last", (number, True, Custom, "abc", EQ) < (1, True, Custom, "abc", GT))
  print ("greater in fourth", (number, True, Custom, "abd", EQ) > (1, True, Custom, "abc", EQ))
  print ("first decides", compare (number, True, Custom, "zzz", GT) (2, False, Custom, "", LT))
  print ("unit", () == (), compare () (), () < ())
  print ("custom", Custom <= Custom, Custom /= Custom, max Custom Custom == Custom)
