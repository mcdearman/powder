{-# LANGUAGE TypeOperators, ExplicitNamespaces #-}
-- That two types are the same, as a value.
module Data.Type.Equality ((:~:) (..), type (~), sym, trans, castWith) where

import Powder.Base
