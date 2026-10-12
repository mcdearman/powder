{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Shapes (Shape, pattern Square, pattern Circle, pattern Origin, pattern Sized, area) where

-- How a shape is kept, which no other module sees.
data Shape = Rect Int Int | Round Int | Point

pattern Square :: Int -> Shape
pattern Square s <- (square -> Just s)
  where Square s = Rect s s

pattern Circle :: Int -> Shape
pattern Circle r = Round r

pattern Origin :: Shape
pattern Origin = Point

-- A pattern only: there is no one shape of a size.
pattern Sized :: Int -> Shape
pattern Sized n <- (area -> n)

square :: Shape -> Maybe Int
square (Rect w h) | w == h = Just w
square _ = Nothing

area :: Shape -> Int
area (Rect w h) = w * h
area (Round r) = 3 * r * r
area Point = 0
