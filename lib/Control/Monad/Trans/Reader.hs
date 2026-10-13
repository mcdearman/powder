{-# LANGUAGE GHC2021 #-}
-- Something a computation is given, and can read wherever it is.
module Control.Monad.Trans.Reader
  ( ReaderT (..), Reader, runReader, mapReader, withReader, mapReaderT, withReaderT, reader, ask, local, asks
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Functor.Identity

newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

type Reader r = ReaderT r Identity

runReader :: Reader r a -> r -> a
runReader m r = runIdentity (runReaderT m r)

mapReader :: (a -> b) -> Reader r a -> Reader r b
mapReader f = mapReaderT (Identity . f . runIdentity)

withReader :: (r' -> r) -> Reader r a -> Reader r' a
withReader = withReaderT

mapReaderT :: (m a -> n b) -> ReaderT r m a -> ReaderT r n b
mapReaderT f m = ReaderT (f . runReaderT m)

withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
withReaderT f m = ReaderT (runReaderT m . f)

instance Functor m => Functor (ReaderT r m) where
  fmap f m = ReaderT (fmap f . runReaderT m)

instance Applicative m => Applicative (ReaderT r m) where
  pure a = ReaderT (const (pure a))
  mf <*> mx = ReaderT (\r -> runReaderT mf r <*> runReaderT mx r)

instance Monad m => Monad (ReaderT r m) where
  m >>= k = ReaderT (\r -> runReaderT m r >>= \a -> runReaderT (k a) r)

instance MonadFail m => MonadFail (ReaderT r m) where
  fail message = ReaderT (const (fail message))

instance Alternative m => Alternative (ReaderT r m) where
  empty = ReaderT (const empty)
  m <|> n = ReaderT (\r -> runReaderT m r <|> runReaderT n r)

instance MonadTrans (ReaderT r) where
  lift m = ReaderT (const m)

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

reader :: Monad m => (r -> a) -> ReaderT r m a
reader f = ReaderT (return . f)

ask :: Monad m => ReaderT r m r
ask = ReaderT return

local :: (r -> r) -> ReaderT r m a -> ReaderT r m a
local = withReaderT

asks :: Monad m => (r -> a) -> ReaderT r m a
asks = reader
