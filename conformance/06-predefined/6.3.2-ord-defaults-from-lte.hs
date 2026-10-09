-- Haskell 2010 Report 6.3.2: class Ord; defining only (<=) gives compare, <, >, >=, max, min by the defaults
data Size = Small | Medium | Large deriving (Eq, Show)

rank :: Size -> Int
rank Small = 0
rank Medium = 1
rank Large = 2

instance Ord Size where
  a <= b = rank a <= rank b

-- ordered by length only; Eq is consistent with that
newtype ByLen = ByLen String deriving Show

instance Eq ByLen where
  ByLen a == ByLen b = length a == length b

instance Ord ByLen where
  ByLen a <= ByLen b = length a <= length b

sizes :: [Size]
sizes = [Small, Medium, Large]

main :: IO ()
main = do
  print ("<= table", [a <= b | a <- sizes, b <- sizes])
  print ("compare table", [compare a b | a <- sizes, b <- sizes])
  print ("< table", [a < b | a <- sizes, b <- sizes])
  print ("> table", [a > b | a <- sizes, b <- sizes])
  print (">= table", [a >= b | a <- sizes, b <- sizes])
  print ("max table", [max a b | a <- sizes, b <- sizes])
  print ("min table", [min a b | a <- sizes, b <- sizes])
  print ("maximum/minimum", maximum [Medium, Large, Small], minimum [Medium, Large, Small])
  print ("ByLen compare", compare (ByLen "ab") (ByLen "xyz"), compare (ByLen "ab") (ByLen "zz"), compare (ByLen "abc") (ByLen ""))
  print ("ByLen operators", ByLen "a" < ByLen "bb", ByLen "aa" < ByLen "bb", ByLen "aa" >= ByLen "bb", ByLen "aaa" > ByLen "bb")
  print ("ByLen max distinct", max (ByLen "a") (ByLen "bb"), min (ByLen "a") (ByLen "bb"))
  print ("in a list", [Small, Large] < [Medium], [Large] > [Medium, Large], compare [Small, Medium] [Small, Medium])
  print ("in a tuple", (Small, 'b') < (Small, 'c'), (Large, 'a') > (Medium, 'z'))
  print ("in Maybe/Either", Just Small < Just Large, (Left Large :: Either Size Size) < Right Small)
