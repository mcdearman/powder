-- Haskell 2010 Report 4.6: kind inference over mutually recursive data, type synonym and class declarations
module Main (main) where

-- Rose and Kids are inferred together: f :: * -> *, a :: *
data Rose f a = Rose a (Kids f a)

newtype Kids f a = Kids (f (Rose f a))

-- the Report's example: D, S and C form one dependency group (D uses C in its context,
-- S uses D, and the class method mentions D)
data C a => D a = Foo (S a)

type S a = [D a]

class C a where
  bar :: a -> D a -> Bool

instance C Int where
  bar x (Foo ds) = length ds == x

-- a class whose parameter has kind (* -> *) -> * -> *, inferred from the method type
data Holder f a = Holder (f a)

class Collapse t where
  collapse :: t f a -> Holder f a

instance Collapse Holder where
  collapse h = h

data Twice f a = Twice (f a) (f a)

instance Collapse Twice where
  collapse (Twice x _) = Holder x

sizeList :: Rose [] a -> Int
sizeList (Rose _ (Kids rs)) = 1 + sum (map sizeList rs)

depthMaybe :: Rose Maybe a -> Int
depthMaybe (Rose _ (Kids Nothing)) = 1
depthMaybe (Rose _ (Kids (Just r))) = 1 + depthMaybe r

leaf :: a -> Rose [] a
leaf x = Rose x (Kids [])

unHolder :: Holder f a -> f a
unHolder (Holder x) = x

main :: IO ()
main = do
  print ("Rose [] size", sizeList (Rose 'a' (Kids [leaf 'b', Rose 'c' (Kids [leaf 'd'])])))
  print ("Rose [] leaf", sizeList (leaf True))
  print ("Rose Maybe depth", depthMaybe (Rose 1 (Kids (Just (Rose (2 :: Int) (Kids Nothing))))))
  print ("Report example", bar (2 :: Int) (Foo [Foo [], Foo []]), bar (0 :: Int) (Foo [Foo []]))
  print ("Collapse Holder", unHolder (collapse (Holder (Just 'h'))))
  print ("Collapse Twice", unHolder (collapse (Twice "first" "second")))
