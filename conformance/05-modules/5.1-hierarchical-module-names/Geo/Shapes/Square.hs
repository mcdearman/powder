module Geo.Shapes.Square (area, viaUtil) where

import Geo.Util (describe)

area :: Int -> Int
area s = s * s

viaUtil :: Int -> String
viaUtil = describe . area
