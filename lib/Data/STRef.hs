{-# LANGUAGE GHC2021 #-}
-- A place in which a value is kept, and changed, in a computation that
-- changes things no one else can see.
module Data.STRef (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef, modifySTRef') where

import Control.Monad.ST (ST, unsafeIOToST)
import Data.IORef

newtype STRef s a = STRef (IORef a)

instance Eq (STRef s a) where
  STRef a == STRef b = a == b

newSTRef :: a -> ST s (STRef s a)
newSTRef x = unsafeIOToST (fmap STRef (newIORef x))

readSTRef :: STRef s a -> ST s a
readSTRef (STRef r) = unsafeIOToST (readIORef r)

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef (STRef r) x = unsafeIOToST (writeIORef r x)

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef (STRef r) f = unsafeIOToST (modifyIORef r f)

modifySTRef' :: STRef s a -> (a -> a) -> ST s ()
modifySTRef' (STRef r) f = unsafeIOToST (modifyIORef' r f)
