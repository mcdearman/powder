-- Haskell 2010 Report 4.3.2: an instance for lists with a context on the element type, for a class with an
-- operator as its method and a fixity declared for it
module Main (main) where

infix 4 ===

class MyEq a where
  (===) :: a -> a -> Bool

data MyData = MyData | Other

instance MyEq MyData where
  MyData === MyData = True
  Other === Other = True
  _ === _ = False

instance MyEq a => MyEq [a] where
  [] === [] = True
  (x : xs) === (y : ys) = x === y && xs === ys
  _ === _ = False

instance (MyEq a, MyEq b) => MyEq (a, b) where
  (a, b) === (c, d) = a === c && b === d

main :: IO ()
main = do
  print ([MyData] === [MyData], [MyData, Other] === [MyData, MyData], [MyData] === [])
  print ([[MyData], []] === [[MyData], []], [[Other]] === [[MyData]])
  print (([MyData], Other) === ([MyData], Other), (MyData, [Other]) === (MyData, []))
  print (not ([] === [MyData]) && [] === ([] :: [MyData]))
