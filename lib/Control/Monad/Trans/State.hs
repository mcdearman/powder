{-# LANGUAGE GHC2021 #-}
-- Something kept and changed as a computation goes: a function from what
-- is kept to a result and what is kept after.
module Control.Monad.Trans.State
  ( StateT (..), State, runState, evalState, execState, mapState, withState, evalStateT, execStateT, mapStateT,
    withStateT, state, get, put, modify, modify', gets
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Functor.Identity

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

type State s = StateT s Identity

runState :: State s a -> s -> (a, s)
runState m s = runIdentity (runStateT m s)

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

execState :: State s a -> s -> s
execState m s = snd (runState m s)

mapState :: ((a, s) -> (b, s)) -> State s a -> State s b
mapState f = mapStateT (Identity . f . runIdentity)

withState :: (s -> s) -> State s a -> State s a
withState = withStateT

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT m s = do
  ~(a, _) <- runStateT m s
  return a

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT m s = do
  ~(_, s') <- runStateT m s
  return s'

mapStateT :: (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapStateT f m = StateT (f . runStateT m)

withStateT :: (s -> s) -> StateT s m a -> StateT s m a
withStateT f m = StateT (runStateT m . f)

instance Functor m => Functor (StateT s m) where
  fmap f m = StateT (\s -> fmap (\ ~(a, s') -> (f a, s')) (runStateT m s))

instance Monad m => Applicative (StateT s m) where
  pure a = StateT (\s -> return (a, s))
  mf <*> mx = StateT $ \s -> do
    ~(f, s') <- runStateT mf s
    ~(x, s'') <- runStateT mx s'
    return (f x, s'')

instance Monad m => Monad (StateT s m) where
  m >>= k = StateT $ \s -> do
    ~(a, s') <- runStateT m s
    runStateT (k a) s'

instance MonadFail m => MonadFail (StateT s m) where
  fail message = StateT (\_ -> fail message)

instance (Monad m, Alternative m) => Alternative (StateT s m) where
  empty = StateT (const empty)
  m <|> n = StateT (\s -> runStateT m s <|> runStateT n s)

instance MonadTrans (StateT s) where
  lift m = StateT (\s -> m >>= \a -> return (a, s))

instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift . liftIO

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT (return . f)

get :: Monad m => StateT s m s
get = state (\s -> (s, s))

put :: Monad m => s -> StateT s m ()
put s = state (const ((), s))

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = state (\s -> ((), f s))

modify' :: Monad m => (s -> s) -> StateT s m ()
modify' f = do
  s <- get
  put $! f s

gets :: Monad m => (s -> a) -> StateT s m a
gets f = state (\s -> (f s, s))
