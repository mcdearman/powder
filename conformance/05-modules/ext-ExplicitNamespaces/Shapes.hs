{-# LANGUAGE ExplicitNamespaces #-}
module Shapes (type Shape (..), type Named, type Size, area) where

data Shape = Circle Int | Box Int Int

type Named = String

type Size = Int

area :: Shape -> Size
area (Circle r) = 3 * r * r
area (Box w h) = w * h
