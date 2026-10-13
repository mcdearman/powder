{-# LANGUAGE GHC2021 #-}
-- Computations that change things no one else can see, and so give the
-- same result each time: run, they are a value. What is changed is of a
-- type that says which run it belongs to, and cannot be let out of it.
module Control.Monad.ST (ST, runST, stToIO, unsafeIOToST) where

newtype ST s a = ST (IO a)

unST :: ST s a -> IO a
unST (ST io) = io

instance Functor (ST s) where
  fmap f (ST io) = ST (fmap f io)

instance Applicative (ST s) where
  pure = ST . return
  ST f <*> ST x = ST (f <*> x)

instance Monad (ST s) where
  ST m >>= k = ST (m >>= unST . k)

runST :: (forall s. ST s a) -> a
runST st = primPerformIO (unST st)

stToIO :: ST s a -> IO a
stToIO = unST

unsafeIOToST :: IO a -> ST s a
unsafeIOToST = ST
