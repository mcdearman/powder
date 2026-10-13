{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}
-- The monads that add to something as they go, whatever they are made
-- over: the class of them, and the one that does only that.
module Control.Monad.Writer
  ( MonadWriter (..), listens, censor,
    Writer, runWriter, execWriter, mapWriter,
    WriterT (WriterT), runWriterT, execWriterT, mapWriterT,
    MonadTrans (..), MonadIO (..),
  )
where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader (ReaderT (..), mapReaderT)
import Control.Monad.Trans.State (StateT (..))
import Control.Monad.Trans.Writer (Writer, WriterT (..), execWriter, execWriterT, mapWriter, mapWriterT, runWriter)
import qualified Control.Monad.Trans.Writer as Trans

class (Monoid w, Monad m) => MonadWriter w m | m -> w where
  writer :: (a, w) -> m a
  writer ~(a, w) = tell w >> return a
  tell :: w -> m ()
  tell w = writer ((), w)
  listen :: m a -> m (a, w)
  pass :: m (a, w -> w) -> m a

listens :: MonadWriter w m => (w -> b) -> m a -> m (a, b)
listens f m = do
  ~(a, w) <- listen m
  return (a, f w)

censor :: MonadWriter w m => (w -> w) -> m a -> m a
censor f m = pass (fmap (\a -> (a, f)) m)

instance (Monoid w, Monad m) => MonadWriter w (WriterT w m) where
  writer = Trans.writer
  tell = Trans.tell
  listen = Trans.listen
  pass = Trans.pass

instance MonadWriter w m => MonadWriter w (ReaderT r m) where
  writer = lift . writer
  tell = lift . tell
  listen = mapReaderT listen
  pass = mapReaderT pass

instance MonadWriter w m => MonadWriter w (StateT s m) where
  writer = lift . writer
  tell = lift . tell
  listen m = StateT $ \s -> do
    ~((a, s'), w) <- listen (runStateT m s)
    return ((a, w), s')
  pass m = StateT $ \s -> pass $ do
    ~((a, f), s') <- runStateT m s
    return ((a, s'), f)

instance MonadWriter w m => MonadWriter w (ExceptT e m) where
  writer = lift . writer
  tell = lift . tell
  listen m = ExceptT $ do
    ~(a, w) <- listen (runExceptT m)
    return (fmap (\x -> (x, w)) a)
  pass m = ExceptT $ pass $ do
    a <- runExceptT m
    return (case a of
      Left e -> (Left e, id)
      Right (x, f) -> (Right x, f))

instance MonadWriter w m => MonadWriter w (MaybeT m) where
  writer = lift . writer
  tell = lift . tell
  listen m = MaybeT $ do
    ~(a, w) <- listen (runMaybeT m)
    return (fmap (\x -> (x, w)) a)
  pass m = MaybeT $ pass $ do
    a <- runMaybeT m
    return (case a of
      Nothing -> (Nothing, id)
      Just (x, f) -> (Just x, f))
