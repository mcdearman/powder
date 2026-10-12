-- The Prelude of the Haskell2010 edition has the Report's names and no others: a program may use for itself the names
-- that later Preludes took. GHC gives -XHaskell2010 its one Prelude, and calls each of these names ambiguous.
infixr 6 <>
infixl 4 <$>, <*>

(<>) :: Int -> Int -> Int
a <> b = a * 10 + b

(<$>) :: (a -> b) -> a -> b
f <$> x = f x

(<*>) :: (a -> b) -> a -> b
f <*> x = f x

pure :: a -> [a]
pure x = [x, x]

mempty :: String
mempty = "nothing"

mappend :: String -> String -> String
mappend a b = a ++ "+" ++ b

mconcat :: [String] -> String
mconcat = foldr mappend mempty

foldMap :: (a -> String) -> [a] -> String
foldMap f = concatMap f

traverse :: (a -> b) -> [a] -> [b]
traverse = map

sequenceA :: [a] -> [a]
sequenceA = reverse

liftA2 :: (a -> b -> c) -> a -> b -> c
liftA2 f = f

data Semigroup = Semigroup deriving Show

data Monoid = Monoid deriving Show

data Applicative = Applicative deriving Show

class Foldable a where
  folded :: a -> String

instance Foldable Bool where
  folded = show

class Traversable a where
  traversed :: a -> String

instance Traversable Char where
  traversed c = [c, c]

main :: IO ()
main = do
  putStrLn ("operators: " ++ show (1 <> 2 <> 3, (+ 1) <$> (1 :: Int), (+) <$> 1 <*> (2 :: Int)))
  putStrLn ("pure: " ++ show (pure 'x'))
  putStrLn ("monoid: " ++ show (mempty, mappend "a" "b", mconcat ["a", "b"]))
  putStrLn ("folds: " ++ show (foldMap show [1, 2, 3 :: Int], traverse (+ 1) [1, 2 :: Int], sequenceA "abc", liftA2 (+) 1 (2 :: Int)))
  putStrLn ("types: " ++ show (Semigroup, Monoid, Applicative))
  putStrLn ("classes: " ++ show (folded True, traversed 'c'))
  -- What the Report's Prelude does have is as it was.
  putStrLn ("report: " ++ show (length "abc", sum [1, 2, 3 :: Int], elem 2 [1, 2 :: Int], fmap (+ 1) (Just (1 :: Int))))
  mapM_ print (sequence [Just 'a', Just 'b'])
  Just x <- return (Just 'j')
  putStrLn ("fail: " ++ show (x, fail "no" :: Maybe Int))
