-- Haskell 2010 Report 4.6: kinds are inferred for type constructors; unconstrained parameters default to kind *
module Main (main) where

-- App :: (* -> *) -> * -> *
data App f a = A (f a)

-- Tree :: * -> * (the parameter is never used, so it defaults to *)
data Tree a = Leaf | Fork (Tree a) (Tree a)

-- Phantom :: * -> *
data Phantom a = Phantom

-- Two :: (* -> *) -> (* -> *) -> * -> *
data Two f g a = Two (f a) (g a)

-- Flip :: (* -> * -> *) -> * -> * -> *
data Flip f a b = Flip (f b a)

-- Compose :: (* -> *) -> (* -> *) -> * -> *
newtype Compose f g a = Compose (f (g a))

-- T3 :: * -> * -> * -> *
data T3 a b c = T3

unA :: App f a -> f a
unA (A x) = x

forks :: Tree a -> Int
forks Leaf = 0
forks (Fork l r) = 1 + forks l + forks r

phantomName :: Phantom a -> String
phantomName Phantom = "phantom"

unTwo :: Two f g a -> (f a, g a)
unTwo (Two x y) = (x, y)

unFlip :: Flip f a b -> f b a
unFlip (Flip x) = x

unCompose :: Compose f g a -> f (g a)
unCompose (Compose x) = x

t3Name :: T3 Int (Tree Bool) (Phantom Char) -> String
t3Name T3 = "t3"

main :: IO ()
main = do
  print ("App Maybe Int", unA (A (Just (1 :: Int))))
  print ("App [] Char", unA (A "abc"))
  print ("Tree Int", forks (Fork (Fork Leaf Leaf) Leaf :: Tree Int))
  print ("Tree Bool", forks (Leaf :: Tree Bool))
  print ("Tree (Maybe [Char])", forks (Fork Leaf Leaf :: Tree (Maybe String)))
  print ("Tree (Tree ())", forks (Fork Leaf (Fork Leaf Leaf) :: Tree (Tree ())))
  print ("Phantom", phantomName (Phantom :: Phantom Int), phantomName (Phantom :: Phantom (Phantom Bool)))
  print ("Two Maybe []", unTwo (Two (Just 'a') "bc"))
  print ("Flip Either", unFlip (Flip (Left "s") :: Flip Either Int String))
  print ("Flip (,)", unFlip (Flip ('a', True)))
  print ("Compose Maybe []", unCompose (Compose (Just [1 :: Int, 2])))
  print ("Compose [] Maybe", unCompose (Compose [Just 'a', Nothing]))
  print ("T3", t3Name T3)
