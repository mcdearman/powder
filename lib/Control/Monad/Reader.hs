{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}
-- The monads that are given something to read, whatever they are made
-- over: the class of them, and the one that does only that.
module Control.Monad.Reader
  ( MonadReader (..), asks,
    Reader, runReader, mapReader, withReader,
    ReaderT (ReaderT), runReaderT, mapReaderT, withReaderT,
    MonadTrans (..), MonadIO (..),
  )
where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (ExceptT, mapExceptT)
import Control.Monad.Trans.Maybe (MaybeT, mapMaybeT)
import Control.Monad.Trans.Reader (Reader, ReaderT (..), mapReader, mapReaderT, runReader, withReader, withReaderT)
import qualified Control.Monad.Trans.Reader as Trans
import Control.Monad.Trans.State (StateT, mapStateT)
import Control.Monad.Trans.Writer (WriterT, mapWriterT)

class Monad m => MonadReader r m | m -> r where
  ask :: m r
  ask = reader id
  local :: (r -> r) -> m a -> m a
  reader :: (r -> a) -> m a
  reader f = fmap f ask

asks :: MonadReader r m => (r -> a) -> m a
asks = reader

instance Monad m => MonadReader r (ReaderT r m) where
  ask = Trans.ask
  local = Trans.local
  reader = Trans.reader

instance MonadReader r m => MonadReader r (StateT s m) where
  ask = lift ask
  local = mapStateT . local
  reader = lift . reader

instance (Monoid w, MonadReader r m) => MonadReader r (WriterT w m) where
  ask = lift ask
  local = mapWriterT . local
  reader = lift . reader

instance MonadReader r m => MonadReader r (ExceptT e m) where
  ask = lift ask
  local = mapExceptT . local
  reader = lift . reader

instance MonadReader r m => MonadReader r (MaybeT m) where
  ask = lift ask
  local = mapMaybeT . local
  reader = lift . reader
