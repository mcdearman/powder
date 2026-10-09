-- Haskell 2010 Report 4.1.1: kinds; type constructors of kind * -> * and * -> * -> * as type parameters
module Main (main) where

data Wrap f a = Wrap (f a)

data Both p a b = Both (p a b) (p b a)

unwrapMaybe :: Wrap Maybe a -> Maybe a
unwrapMaybe (Wrap m) = m

unwrapList :: Wrap [] a -> [a]
unwrapList (Wrap xs) = xs

mapWrap :: Functor f => (a -> b) -> Wrap f a -> Wrap f b
mapWrap g (Wrap x) = Wrap (fmap g x)

firstOf :: Both (,) a b -> (a, b)
firstOf (Both x _) = x

secondOf :: Both (,) a b -> (b, a)
secondOf (Both _ y) = y

eitherBoth :: Both Either Int Bool -> (Either Int Bool, Either Bool Int)
eitherBoth (Both x y) = (x, y)

applyBoth :: Both (->) a a -> a -> a
applyBoth (Both f g) x = g (f x)

main :: IO ()
main = do
  print ("wrap maybe", unwrapMaybe (Wrap (Just (3 :: Int))))
  print ("wrap list", unwrapList (Wrap "abc"))
  print ("map maybe", unwrapMaybe (mapWrap (+ 1) (Wrap (Just (41 :: Int)))))
  print ("map list", unwrapList (mapWrap not (Wrap [True, False])))
  print ("map nothing", unwrapMaybe (mapWrap (+ 1) (Wrap (Nothing :: Maybe Int))))
  print ("both first", firstOf (Both (1 :: Int, 'a') ('b', 2)))
  print ("both second", secondOf (Both (1 :: Int, 'a') ('b', 2)))
  print ("either both", eitherBoth (Both (Left 1) (Left True)))
  print ("either both right", eitherBoth (Both (Right False) (Right 7)))
  print ("function both", applyBoth (Both (+ 1) (* 2)) (5 :: Int))
