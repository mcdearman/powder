{-# LANGUAGE GHC2021 #-}
-- Something a computation adds to as it goes.
module Control.Monad.Trans.Writer
  ( WriterT (..), Writer, runWriter, execWriter, mapWriter, execWriterT, mapWriterT, writer, tell, listen,
    listens, pass, censor
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Functor.Identity

newtype WriterT w m a = WriterT {runWriterT :: m (a, w)}

type Writer w = WriterT w Identity

runWriter :: Writer w a -> (a, w)
runWriter = runIdentity . runWriterT

execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)

mapWriter :: ((a, w) -> (b, w')) -> Writer w a -> Writer w' b
mapWriter f = mapWriterT (Identity . f . runIdentity)

execWriterT :: Monad m => WriterT w m a -> m w
execWriterT m = do
  ~(_, w) <- runWriterT m
  return w

mapWriterT :: (m (a, w) -> n (b, w')) -> WriterT w m a -> WriterT w' n b
mapWriterT f m = WriterT (f (runWriterT m))

instance Functor m => Functor (WriterT w m) where
  fmap f m = WriterT (fmap (\ ~(a, w) -> (f a, w)) (runWriterT m))

instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
  pure a = WriterT (pure (a, mempty))
  mf <*> mx = WriterT ((\ ~(f, w) ~(x, w') -> (f x, w <> w')) <$> runWriterT mf <*> runWriterT mx)

instance (Monoid w, Monad m) => Monad (WriterT w m) where
  m >>= k = WriterT $ do
    ~(a, w) <- runWriterT m
    ~(b, w') <- runWriterT (k a)
    return (b, w <> w')

instance (Monoid w, MonadFail m) => MonadFail (WriterT w m) where
  fail message = WriterT (fail message)

instance (Monoid w, Alternative m) => Alternative (WriterT w m) where
  empty = WriterT empty
  m <|> n = WriterT (runWriterT m <|> runWriterT n)

instance Monoid w => MonadTrans (WriterT w) where
  lift m = WriterT (m >>= \a -> return (a, mempty))

instance (Monoid w, MonadIO m) => MonadIO (WriterT w m) where
  liftIO = lift . liftIO

writer :: Monad m => (a, w) -> WriterT w m a
writer = WriterT . return

tell :: Monad m => w -> WriterT w m ()
tell w = writer ((), w)

listen :: Monad m => WriterT w m a -> WriterT w m (a, w)
listen m = WriterT $ do
  ~(a, w) <- runWriterT m
  return ((a, w), w)

listens :: Monad m => (w -> b) -> WriterT w m a -> WriterT w m (a, b)
listens f m = WriterT $ do
  ~(a, w) <- runWriterT m
  return ((a, f w), w)

pass :: Monad m => WriterT w m (a, w -> w) -> WriterT w m a
pass m = WriterT $ do
  ~((a, f), w) <- runWriterT m
  return (a, f w)

censor :: Monad m => (w -> w) -> WriterT w m a -> WriterT w m a
censor f m = WriterT $ do
  ~(a, w) <- runWriterT m
  return (a, f w)
