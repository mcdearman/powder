{-# LANGUAGE GHC2021 #-}
-- A computation that may have no result.
module Control.Monad.Trans.Maybe (MaybeT (..), mapMaybeT, hoistMaybe, maybeToExceptT, exceptToMaybeT) where

import Control.Applicative (Alternative (..))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

mapMaybeT :: (m (Maybe a) -> n (Maybe b)) -> MaybeT m a -> MaybeT n b
mapMaybeT f = MaybeT . f . runMaybeT

hoistMaybe :: Applicative m => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure

maybeToExceptT :: Functor m => e -> MaybeT m a -> ExceptT e m a
maybeToExceptT e (MaybeT m) = ExceptT (fmap (maybe (Left e) Right) m)

exceptToMaybeT :: Functor m => ExceptT e m a -> MaybeT m a
exceptToMaybeT (ExceptT m) = MaybeT (fmap (either (const Nothing) Just) m)

instance Functor m => Functor (MaybeT m) where
  fmap f = mapMaybeT (fmap (fmap f))

instance Monad m => Applicative (MaybeT m) where
  pure = MaybeT . return . Just
  mf <*> mx = MaybeT $ do
    f <- runMaybeT mf
    case f of
      Nothing -> return Nothing
      Just g -> do
        x <- runMaybeT mx
        case x of
          Nothing -> return Nothing
          Just a -> return (Just (g a))

instance Monad m => Monad (MaybeT m) where
  m >>= k = MaybeT $ do
    a <- runMaybeT m
    case a of
      Nothing -> return Nothing
      Just x -> runMaybeT (k x)

instance Monad m => MonadFail (MaybeT m) where
  fail _ = MaybeT (return Nothing)

instance Monad m => Alternative (MaybeT m) where
  empty = MaybeT (return Nothing)
  m <|> n = MaybeT $ do
    a <- runMaybeT m
    case a of
      Nothing -> runMaybeT n
      Just x -> return (Just x)

instance MonadTrans MaybeT where
  lift = MaybeT . fmap Just

instance MonadIO m => MonadIO (MaybeT m) where
  liftIO = lift . liftIO
