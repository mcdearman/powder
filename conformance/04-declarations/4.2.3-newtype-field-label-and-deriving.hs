-- Haskell 2010 Report 4.2.3: newtype with a field label, and deriving standard classes on newtypes
module Main (main) where

newtype Age = Age { getAge :: Int } deriving (Eq, Ord, Show)

newtype Name = Name String deriving (Eq, Ord, Show, Read)

newtype Box a = Box a deriving (Eq, Show)

newtype Count = Count Int deriving (Eq, Ord, Show, Bounded)

newtype Pairs a b = Pairs { unPairs :: [(a, b)] } deriving (Eq, Show)

main :: IO ()
main = do
  print ("show record newtype", Age 3)
  print ("show plain newtype", Name "bob")
  print ("show negative payload", Box (-1 :: Int))
  print ("show nested", Box (Box 'c'))
  print ("show in Just", Just (Age 4))
  print ("eq", Age 3 == Age 3, Age 3 /= Age 4)
  print ("ord", Age 3 < Age 4, compare (Name "b") (Name "a"), max (Age 1) (Age 2))
  print ("selector", getAge (Age 30))
  print ("record construction", Age { getAge = 5 })
  print ("record update", (Age 5) { getAge = 6 })
  print ("record pattern", case Age 7 of Age { getAge = n } -> n)
  print ("read", read "Name \"amy\"" :: Name)
  print ("read spaces", read "  Name   \"x\" " :: Name)
  print ("bounded", minBound :: Count, maxBound :: Count)
  print ("parameterised record", Pairs [(1 :: Int, 'a')])
  print ("parameterised selector", unPairs (Pairs [(True, "t")]))
