-- Haskell 2010 Report 6.3.5: user-defined Functor instances (tree, pair of same type, wrapped function, partially applied type)
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

data Two a = Two a a deriving (Eq, Show)

instance Functor Two where
  fmap f (Two x y) = Two (f x) (f y)

newtype Fn r a = Fn (r -> a)

instance Functor (Fn r) where
  fmap f (Fn g) = Fn (f . g)

runFn :: Fn r a -> r -> a
runFn (Fn g) = g

-- a functor in its last parameter, the first is carried along
data Tagged t a = Tagged t a deriving (Eq, Show)

instance Functor (Tagged t) where
  fmap f (Tagged t x) = Tagged t (f x)

newtype Box a = Box a deriving (Eq, Show)

instance Functor Box where
  fmap f (Box x) = Box (f x)

tree :: Tree Int
tree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

toList :: Tree a -> [a]
toList Leaf = []
toList (Node l x r) = toList l ++ [x] ++ toList r

incAll :: Functor f => f Int -> f Int
incAll = fmap (+ 1)

main :: IO ()
main = do
  print (fmap (* 10) tree)
  print ("tree", toList (fmap show tree), fmap (+ 1) (Leaf :: Tree Int))
  print ("tree identity law", fmap id tree == tree)
  print ("tree composition law", fmap (show . (+ 1)) tree == (fmap show . fmap (+ 1)) tree)
  print ("two", fmap not (Two True False), fmap length (Two "a" "bcd"))
  print ("two laws", fmap id (Two 'a' 'b') == Two 'a' 'b', fmap (succ . succ) (Two 'a' 'b') == fmap succ (fmap succ (Two 'a' 'b')))
  print ("fn", runFn (fmap (+ 1) (Fn length)) "abc", runFn (fmap show (Fn not)) True)
  print ("fn composition", runFn (fmap (* 2) (fmap (+ 1) (Fn fromEnum))) 'a')
  print ("tagged", fmap (+ 1) (Tagged "t" (1 :: Int)), fmap show (Tagged 'k' True))
  print ("box", fmap (+ 1) (Box (1 :: Int)), fmap (fmap not) (Box (Just True)))
  print ("overloaded", incAll (Box 1), incAll (Two 1 2), toList (incAll tree), incAll (Tagged () 5))
  print ("mixed with standard functors", fmap (fmap (+ 1)) [Box 1, Box (2 :: Int)], fmap toList (Just tree))
  print ("lazy", case fmap (const 'x') (Box (undefined :: Int)) of { Box c -> c })
