{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}
-- The monads that may stop with something in place of a result, whatever
-- they are made over: the class of them, and the one that does only that.
module Control.Monad.Except
  ( MonadError (..), liftEither, tryError, withError, handleError, mapError,
    Except, runExcept, mapExcept, withExcept,
    ExceptT (ExceptT), runExceptT, mapExceptT, withExceptT,
  )
where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (Except, ExceptT (..), mapExcept, mapExceptT, runExcept, runExceptT, withExcept, withExceptT)
import qualified Control.Monad.Trans.Except as Trans
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.State (StateT (..))
import Control.Monad.Trans.Writer (WriterT (..))

class Monad m => MonadError e m | m -> e where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a

liftEither :: MonadError e m => Either e a -> m a
liftEither = either throwError return

tryError :: MonadError e m => m a -> m (Either e a)
tryError m = catchError (fmap Right m) (return . Left)

withError :: MonadError e m => (e -> e) -> m a -> m a
withError f m = tryError m >>= either (throwError . f) return

handleError :: MonadError e m => (e -> m a) -> m a -> m a
handleError = flip catchError

mapError :: (MonadError e m, MonadError e' n) => (m (Either e a) -> n (Either e' b)) -> m a -> n b
mapError f m = f (tryError m) >>= liftEither

instance MonadError e (Either e) where
  throwError = Left
  catchError (Left e) h = h e
  catchError (Right a) _ = Right a

instance Monad m => MonadError e (ExceptT e m) where
  throwError = Trans.throwE
  catchError = Trans.catchE

instance MonadError e m => MonadError e (ReaderT r m) where
  throwError = lift . throwError
  catchError m h = ReaderT (\r -> catchError (runReaderT m r) (\e -> runReaderT (h e) r))

instance MonadError e m => MonadError e (StateT s m) where
  throwError = lift . throwError
  catchError m h = StateT (\s -> catchError (runStateT m s) (\e -> runStateT (h e) s))

instance (Monoid w, MonadError e m) => MonadError e (WriterT w m) where
  throwError = lift . throwError
  catchError m h = WriterT (catchError (runWriterT m) (runWriterT . h))

instance MonadError e m => MonadError e (MaybeT m) where
  throwError = lift . throwError
  catchError m h = MaybeT (catchError (runMaybeT m) (runMaybeT . h))
