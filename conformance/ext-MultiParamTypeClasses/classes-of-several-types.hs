{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
-- MultiParamTypeClasses and FunctionalDependencies: a class of several types, some of which say what others are;
-- with FlexibleInstances and FlexibleContexts, an instance at any types and a class asked of any
module Main (main) where

import Data.List (sort)

-- A class of two types, with nothing said of how they depend.
class Convert a b where
  convert :: a -> b

instance Convert Int String where
  convert = show

instance Convert Int Double where
  convert = fromIntegral

instance Convert Bool Int where
  convert b = if b then 1 else 0

instance Convert a b => Convert [a] [b] where
  convert = map convert

-- One type decides another: the type of a container says what it holds.
class Container f a | f -> a where
  empty :: f
  insert :: a -> f -> f
  toL :: f -> [a]

newtype IntBag = IntBag [Int]

newtype Chars = Chars String

instance Container IntBag Int where
  empty = IntBag []
  insert x (IntBag xs) = IntBag (x : xs)
  toL (IntBag xs) = sort xs

instance Container Chars Char where
  empty = Chars ""
  insert c (Chars s) = Chars (c : s)
  toL (Chars s) = s

instance Container [a] a where
  empty = []
  insert = (:)
  toL = id

-- The dependency says what is inserted, so nothing more need be said.
filled :: Container f a => [a] -> f
filled = foldr insert empty

-- A class with something above it of other types of its own.
class (Show a, Show b) => Pairing a b where
  pairing :: a -> b -> String
  pairing a b = show a ++ "&" ++ show b

instance Pairing Int Bool

instance Pairing Char Char where
  pairing a b = [a, b]

-- A state monad's class, as libraries have it: the monad says the state.
class Monad m => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()

newtype State s a = State (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (State f) = f

instance Functor (State s) where
  fmap f (State g) = State (\s -> case g s of (a, s') -> (f a, s'))

instance Applicative (State s) where
  pure a = State (\s -> (a, s))
  State f <*> State g = State (\s -> case f s of (h, s') -> case g s' of (a, s'') -> (h a, s''))

instance Monad (State s) where
  State g >>= k = State (\s -> case g s of (a, s') -> runState (k a) s')

instance MonadState s (State s) where
  get = State (\s -> (s, s))
  put s = State (const ((), s))

modify :: MonadState s m => (s -> s) -> m ()
modify f = get >>= put . f

counter :: (MonadState Int m) => m Int
counter = do
  n <- get
  put (n + 1)
  return n

-- A class of no types at all.
class Config where
  verbose :: Bool

instance Config where
  verbose = True

-- A class asked of something other than a variable.
shownList :: Show [a] => a -> String
shownList x = show [x, x]

total :: (Num a, Convert a Double) => [a] -> Double
total = sum . map convert

main :: IO ()
main = do
  putStrLn (convert (5 :: Int))
  print (convert (5 :: Int) :: Double, convert True :: Int)
  print (convert [1, 2, 3 :: Int] :: [String], convert [[True], [False, True]] :: [[Int]])
  print (toL (insert 3 (insert 1 (insert 2 empty)) :: IntBag))
  putStrLn (toL (insert 'a' (insert 'b' empty) :: Chars))
  print (toL (filled [3, 1, 2] :: IntBag), toL (filled "xyz" :: Chars), toL (filled [True, False] :: [Bool]))
  putStrLn (pairing (1 :: Int) True ++ " " ++ pairing 'a' 'b')
  print (runState (counter >> counter >> modify (* 10) >> counter) 0)
  print (verbose, shownList 'x', total [1, 2, 3 :: Int])
