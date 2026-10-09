-- Haskell 2010 Report 4.3.2: user instances of the constructor class Functor
module Main (main) where

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Eq, Show)

data Pair a b = Pair a b deriving (Eq, Show)

newtype Fn r a = Fn (r -> a)

data Rose a = Rose a [Rose a] deriving (Eq, Show)

newtype Comp f g a = Comp (f (g a))

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap g (Node l v r) = Node (fmap g l) (g v) (fmap g r)

-- the last type parameter is the one mapped over
instance Functor (Pair a) where
  fmap g (Pair a b) = Pair a (g b)

instance Functor (Fn r) where
  fmap g (Fn f) = Fn (g . f)

instance Functor Rose where
  fmap g (Rose a kids) = Rose (g a) (map (fmap g) kids)

instance (Functor f, Functor g) => Functor (Comp f g) where
  fmap h (Comp x) = Comp (fmap (fmap h) x)

runFn :: Fn r a -> r -> a
runFn (Fn f) = f

unComp :: Comp f g a -> f (g a)
unComp (Comp x) = x

double :: Functor f => f Int -> f Int
double = fmap (* 2)

tree :: Tree Int
tree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

main :: IO ()
main = do
  print ("tree", fmap (+ 1) tree)
  print ("tree change type", fmap even tree)
  print ("tree leaf", fmap not (Leaf :: Tree Bool))
  print ("pair", fmap show (Pair 'k' (1 :: Int)))
  print ("fn", runFn (fmap (+ 1) (Fn length)) "abc")
  print ("fn twice", runFn (fmap show (fmap (* 2) (Fn (+ 1)))) (20 :: Int))
  print ("rose", fmap succ (Rose 'a' [Rose 'b' [], Rose 'c' [Rose 'd' []]]))
  print ("comp", unComp (fmap (+ 1) (Comp [Just 1, Nothing, Just (3 :: Int)])))
  print ("comp tree of lists", unComp (fmap not (Comp (Node Leaf [True, False] Leaf))))
  print ("generic", double tree == fmap (* 2) tree, double [1, 2], double (Just 4))
  print ("identity law", fmap id tree == tree)
  print ("composition law", fmap (show . (+ 1)) tree == (fmap show . fmap (+ 1)) tree)
