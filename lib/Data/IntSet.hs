-- Sets of Int: here the sets of Data.Set at that type, and not a type of
-- their own.
module Data.IntSet (IntSet, Key, module Data.Set) where

import Data.Set

type IntSet = Set Int

type Key = Int
