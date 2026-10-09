-- Haskell 2010 Report 4.1.3: class assertions and contexts, including the C (m t) form and the empty context
module Main (main) where

showInside :: Show (m a) => m a -> String
showInside x = "<" ++ show x ++ ">"

showBoth :: (Show (f a), Show (f b)) => f a -> f b -> String
showBoth x y = show x ++ "/" ++ show y

eqLifted :: Eq (m Int) => m Int -> m Int -> Bool
eqLifted x y = x == y

mapAndCompare :: (Functor f, Eq (f b)) => (a -> b) -> f a -> f b -> Bool
mapAndCompare g x y = fmap g x == y

single :: Eq a => a -> a -> Bool
single x y = x == y

parenthesised :: (Eq a) => a -> a -> Bool
parenthesised x y = x /= y

several :: (Eq a, Show a, Num a) => a -> String
several x = if x == 0 then "zero" else show (x + 1)

emptyContext :: () => Int -> Int
emptyContext x = x + 1

qualifiedClass :: Prelude.Show a => a -> String
qualifiedClass = show

class (Eq a, Show a) => Token a where
  render :: a -> String
  render x = "tok:" ++ show x

instance Token Bool

instance Token Int where
  render n = "int:" ++ show n

instance (Token a, Token b) => Token (a, b) where
  render (x, y) = render x ++ "&" ++ render y

instance Token a => Token [a] where
  render xs = concatMap render xs

main :: IO ()
main = do
  print ("showInside maybe", showInside (Just (1 :: Int)))
  print ("showInside list", showInside [True])
  print ("showBoth maybe", showBoth (Just 'a') (Just True))
  print ("showBoth list", showBoth [1 :: Int] "x")
  print ("eqLifted maybe", eqLifted (Just 1) (Just 1))
  print ("eqLifted list", eqLifted [1, 2] [1, 3])
  print ("mapAndCompare list", mapAndCompare (+ 1) [1, 2 :: Int] [2, 3])
  print ("mapAndCompare maybe", mapAndCompare show (Just (1 :: Int)) (Just "1"))
  print ("single", single 'a' 'a')
  print ("parenthesised", parenthesised 'a' 'b')
  print ("several", several (0 :: Int), several (1.5 :: Double))
  print ("emptyContext", emptyContext 1)
  print ("qualifiedClass", qualifiedClass (1 :: Int))
  print ("render default", render True)
  print ("render int", render (3 :: Int))
  print ("render pair", render (True, 1 :: Int))
  print ("render list", render [True, False])
  print ("render nested", render [(1 :: Int, [False])])
