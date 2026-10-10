-- Haskell 2010 Report 4.3.2: a method's clauses in an instance must have the method's type at the
-- instance's type; True is not a pattern of MyData
class MyEq a where
  (===) :: a -> a -> Bool

data MyData = MyData

instance MyEq MyData where
  MyData === True = True

main :: IO ()
main = print (MyData === MyData)
