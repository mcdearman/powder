-- Maps from Int: here the maps of Data.Map at that type of key, and not
-- a type of their own.
module Data.IntMap (IntMap, Key, module Data.Map) where

import Data.Map

type IntMap = Map Int

type Key = Int
