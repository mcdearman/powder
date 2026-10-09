-- Haskell 2010 Report 6.3.1: class Eq; (==) and (/=) are defined in terms of each other, either one suffices
data OnlyEq = OnlyEq Int

instance Eq OnlyEq where
  OnlyEq x == OnlyEq y = x == y

data OnlyNe = OnlyNe Int

instance Eq OnlyNe where
  OnlyNe x /= OnlyNe y = x /= y

-- equality modulo 10: not structural
newtype Mod10 = Mod10 Int

instance Eq Mod10 where
  Mod10 a == Mod10 b = a `mod` 10 == b `mod` 10

data Tree a = Leaf | Node (Tree a) a (Tree a)

instance Eq a => Eq (Tree a) where
  Leaf == Leaf = True
  Node l x r == Node l' x' r' = x == x' && l == l' && r == r'
  _ == _ = False

-- both methods given, deliberately inconsistent: each is used as written
data Odd = Odd

instance Eq Odd where
  _ == _ = True
  _ /= _ = True

allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (x : xs) = all (== x) xs

t1 :: Tree Int
t1 = Node (Node Leaf 1 Leaf) 2 Leaf

t2 :: Tree Int
t2 = Node (Node Leaf 1 Leaf) 3 Leaf

main :: IO ()
main = do
  print ("only ==", OnlyEq 1 == OnlyEq 1, OnlyEq 1 == OnlyEq 2)
  print ("default /=", OnlyEq 1 /= OnlyEq 1, OnlyEq 1 /= OnlyEq 2)
  print ("only /=", OnlyNe 1 /= OnlyNe 1, OnlyNe 1 /= OnlyNe 2)
  print ("default ==", OnlyNe 1 == OnlyNe 1, OnlyNe 1 == OnlyNe 2)
  print ("non-structural", Mod10 3 == Mod10 13, Mod10 3 /= Mod10 13, Mod10 3 == Mod10 4)
  print ("instance with context", t1 == t1, t1 == t2, t1 /= t2, (Leaf :: Tree Int) == Leaf)
  print ("both methods as written", Odd == Odd, Odd /= Odd)
  print ("overloaded use", allSame [OnlyEq 2, OnlyEq 2], allSame [OnlyNe 2, OnlyNe 3], allSame [Mod10 1, Mod10 11, Mod10 21])
  print ("through list instance", [OnlyEq 1, OnlyEq 2] == [OnlyEq 1, OnlyEq 2], [OnlyNe 1] /= [OnlyNe 1])
  print ("through tuple and Maybe", (OnlyEq 1, Mod10 5) == (OnlyEq 1, Mod10 15), Just (OnlyNe 4) == Just (OnlyNe 5))
  print ("elem and lookup", Mod10 7 `elem` [Mod10 1, Mod10 17], lookup (Mod10 12) [(Mod10 1, 'a'), (Mod10 2, 'b')])
  print ("methods as functions", (==) (OnlyEq 5) (OnlyEq 5), (/=) (OnlyNe 5) (OnlyNe 5))
