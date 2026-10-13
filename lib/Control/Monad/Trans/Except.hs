{-# LANGUAGE GHC2021 #-}
-- A computation that may stop with something in place of its result.
module Control.Monad.Trans.Except
  ( ExceptT (..), Except, runExcept, mapExcept, withExcept, runExceptT, mapExceptT, withExceptT, except, throwE,
    catchE, handleE, tryE, finallyE
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Functor.Identity

newtype ExceptT e m a = ExceptT (m (Either e a))

type Except e = ExceptT e Identity

runExcept :: Except e a -> Either e a
runExcept = runIdentity . runExceptT

mapExcept :: (Either e a -> Either e' b) -> Except e a -> Except e' b
mapExcept f = mapExceptT (Identity . f . runIdentity)

withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept = withExceptT

runExceptT :: ExceptT e m a -> m (Either e a)
runExceptT (ExceptT m) = m

mapExceptT :: (m (Either e a) -> n (Either e' b)) -> ExceptT e m a -> ExceptT e' n b
mapExceptT f m = ExceptT (f (runExceptT m))

withExceptT :: Functor m => (e -> e') -> ExceptT e m a -> ExceptT e' m a
withExceptT f = mapExceptT (fmap (either (Left . f) Right))

instance Functor m => Functor (ExceptT e m) where
  fmap f = ExceptT . fmap (fmap f) . runExceptT

instance Monad m => Applicative (ExceptT e m) where
  pure a = ExceptT (return (Right a))
  ExceptT mf <*> ExceptT mx = ExceptT $ do
    f <- mf
    case f of
      Left e -> return (Left e)
      Right g -> do
        x <- mx
        case x of
          Left e -> return (Left e)
          Right a -> return (Right (g a))

instance Monad m => Monad (ExceptT e m) where
  m >>= k = ExceptT $ do
    a <- runExceptT m
    case a of
      Left e -> return (Left e)
      Right x -> runExceptT (k x)

instance MonadFail m => MonadFail (ExceptT e m) where
  fail = ExceptT . fail

instance (Monad m, Monoid e) => Alternative (ExceptT e m) where
  empty = ExceptT (return (Left mempty))
  ExceptT m <|> ExceptT n = ExceptT $ do
    a <- m
    case a of
      Left e -> fmap (either (Left . mappend e) Right) n
      Right x -> return (Right x)

instance MonadTrans (ExceptT e) where
  lift m = ExceptT (fmap Right m)

instance MonadIO m => MonadIO (ExceptT e m) where
  liftIO = lift . liftIO

except :: Monad m => Either e a -> ExceptT e m a
except = ExceptT . return

throwE :: Monad m => e -> ExceptT e m a
throwE = ExceptT . return . Left

catchE :: Monad m => ExceptT e m a -> (e -> ExceptT e' m a) -> ExceptT e' m a
catchE m h = ExceptT $ do
  a <- runExceptT m
  case a of
    Left e -> runExceptT (h e)
    Right x -> return (Right x)

handleE :: Monad m => (e -> ExceptT e' m a) -> ExceptT e m a -> ExceptT e' m a
handleE = flip catchE

tryE :: Monad m => ExceptT e m a -> ExceptT e m (Either e a)
tryE m = catchE (fmap Right m) (return . Left)

finallyE :: Monad m => ExceptT e m a -> ExceptT e m () -> ExceptT e m a
finallyE m closer = do
  result <- tryE m
  closer
  either throwE return result
