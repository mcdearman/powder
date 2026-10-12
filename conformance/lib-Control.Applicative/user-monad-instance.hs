{-# LANGUAGE GHC2021 #-}
-- A monad written by the program: instances of Functor, Applicative, Monad and MonadFail, and do notation over them.
-- In an edition after Haskell 2010, whose Prelude has the classes and their operators.
import Control.Monad

newtype State s a = State (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (State f) = f

instance Functor (State s) where
  fmap f (State g) = State $ \s -> case g s of (a, s') -> (f a, s')

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  State f <*> State g = State $ \s -> case f s of
    (h, s') -> case g s' of
      (a, s'') -> (h a, s'')

instance Monad (State s) where
  State g >>= k = State $ \s -> case g s of (a, s') -> runState (k a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

fresh :: State Int Int
fresh = do
  n <- get
  put (n + 1)
  return n

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

label :: Tree a -> State Int (Tree (Int, a))
label Leaf = return Leaf
label (Node l x r) = do
  l' <- label l
  n <- fresh
  r' <- label r
  return (Node l' (n, x) r')

-- A monad in which a failed pattern is a value.
newtype Result a = Result (Either String a) deriving Show

instance Functor Result where
  fmap f (Result e) = Result (fmap f e)

instance Applicative Result where
  pure = Result . Right
  Result f <*> Result a = Result (f <*> a)

instance Monad Result where
  Result (Left e) >>= _ = Result (Left e)
  Result (Right a) >>= k = k a

instance MonadFail Result where
  fail _ = Result (Left "failed")

firstOf :: [a] -> Result a
firstOf xs = do
  x : _ <- return xs
  return x

main :: IO ()
main = do
  putStrLn ("fresh: " ++ show (runState (replicateM 3 fresh) 10))
  putStrLn ("label: " ++ show (fst (runState (label (Node (Node Leaf 'a' Leaf) 'b' (Node Leaf 'c' Leaf))) 0)))
  putStrLn ("applicative: " ++ show (runState ((,) <$> fresh <*> fresh) 5))
  putStrLn ("then: " ++ show (runState (fresh *> fresh <* fresh) 0))
  putStrLn ("mapM: " ++ show (runState (mapM (\x -> modify (+ x) >> get) [1, 2, 3]) 0))
  putStrLn ("forM_: " ++ show (snd (runState (forM_ [1 .. 10] (\x -> modify (* x))) 1)))
  putStrLn ("when: " ++ show (snd (runState (when True (put 'y')) 'n'), snd (runState (unless True (put 'y')) 'n')))
  putStrLn ("foldM: " ++ show (runState (foldM (\acc x -> modify (+ 1) >> return (acc + x)) 0 [1, 2, 3]) 0))
  putStrLn ("return-is-pure: " ++ show (fst (runState (return 'r') ()), fst (runState (pure 'p') ())))
  putStrLn ("fail-found: " ++ show (firstOf "abc"))
  putStrLn ("fail-missing: " ++ show (firstOf ""))
  putStrLn ("fail-maybe: " ++ show (do { Just x <- [Just 1, Nothing, Just (3 :: Int)]; return x }))
  putStrLn ("fail-direct: " ++ show (fail "no" :: Maybe Int, fail "no" :: [Int]))
