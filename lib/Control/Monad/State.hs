{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}
-- The monads that keep something and change it, whatever they are made
-- over: the class of them, and the one that does only that.
module Control.Monad.State
  ( MonadState (..), modify, modify', gets,
    State, runState, evalState, execState, mapState, withState,
    StateT (StateT), runStateT, evalStateT, execStateT, mapStateT, withStateT,
    MonadTrans (..), MonadIO (..),
  )
where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State (State, StateT (..), evalState, evalStateT, execState, execStateT, mapState, mapStateT, runState, withState, withStateT)
import qualified Control.Monad.Trans.State as Trans
import Control.Monad.Trans.Writer (WriterT)

class Monad m => MonadState s m | m -> s where
  get :: m s
  get = state (\s -> (s, s))
  put :: s -> m ()
  put s = state (const ((), s))
  state :: (s -> (a, s)) -> m a
  state f = do
    s <- get
    let ~(a, s') = f s
    put s'
    return a

modify :: MonadState s m => (s -> s) -> m ()
modify f = state (\s -> ((), f s))

modify' :: MonadState s m => (s -> s) -> m ()
modify' f = do
  s <- get
  put $! f s

gets :: MonadState s m => (s -> a) -> m a
gets f = fmap f get

instance Monad m => MonadState s (StateT s m) where
  get = Trans.get
  put = Trans.put
  state = Trans.state

instance MonadState s m => MonadState s (ReaderT r m) where
  get = lift get
  put = lift . put
  state = lift . state

instance (Monoid w, MonadState s m) => MonadState s (WriterT w m) where
  get = lift get
  put = lift . put
  state = lift . state

instance MonadState s m => MonadState s (ExceptT e m) where
  get = lift get
  put = lift . put
  state = lift . state

instance MonadState s m => MonadState s (MaybeT m) where
  get = lift get
  put = lift . put
  state = lift . state
