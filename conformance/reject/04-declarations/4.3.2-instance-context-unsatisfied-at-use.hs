-- Haskell 2010 Report 4.3.2: an instance for lists that asks for one for the elements is of no use at an
-- element type without one
class MyEq a where
  (===) :: a -> a -> Bool

data MyData = MyData

instance MyEq a => MyEq [a] where
  _ === _ = True

main :: IO ()
main = print ([MyData] === [MyData])
