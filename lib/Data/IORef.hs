-- A place in which a value is kept, and changed, by actions.
module Data.IORef
  ( IORef, newIORef, readIORef, writeIORef, modifyIORef, modifyIORef', atomicModifyIORef, atomicModifyIORef',
    atomicWriteIORef
  )
where

import Powder.Base (Ref (..), newRef, readRef, writeRef)

newtype IORef a = IORef (Ref a)

instance Eq (IORef a) where
  IORef (Ref a) == IORef (Ref b) = a == b

newIORef :: a -> IO (IORef a)
newIORef x = fmap IORef (newRef x)

readIORef :: IORef a -> IO a
readIORef (IORef r) = readRef r

writeIORef :: IORef a -> a -> IO ()
writeIORef (IORef r) x = writeRef r x

modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef r f = readIORef r >>= \x -> writeIORef r (f x)

modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' r f = readIORef r >>= \x -> let x' = f x in x' `seq` writeIORef r x'

-- There is one thread, so every change is one no other comes between.
atomicModifyIORef :: IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef r f = readIORef r >>= \x -> case f x of
  (x', b) -> writeIORef r x' >> return b

atomicModifyIORef' :: IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef' r f = readIORef r >>= \x -> case f x of
  (x', b) -> x' `seq` b `seq` (writeIORef r x' >> return b)

atomicWriteIORef :: IORef a -> a -> IO ()
atomicWriteIORef = writeIORef
