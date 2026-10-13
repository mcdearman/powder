{-# LANGUAGE TypeOperators #-}
-- What a type is, asked for when a program runs.
module Data.Typeable
  ( Typeable,
    TypeRep,
    TyCon,
    typeOf,
    typeRep,
    cast,
    eqT,
    gcast,
    typeRepTyCon,
    typeRepArgs,
    splitTyConApp,
    tyConName,
    tyConModule,
    Proxy (..),
    (:~:) (..),
  )
where

import Powder.Base
