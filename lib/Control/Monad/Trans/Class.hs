{-# LANGUAGE GHC2021 #-}
-- A monad made over another: what the other does, done in it.
module Control.Monad.Trans.Class (MonadTrans (..)) where

class MonadTrans t where
  lift :: Monad m => m a -> t m a
