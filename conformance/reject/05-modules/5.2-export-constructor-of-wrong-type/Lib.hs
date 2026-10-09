module Lib (visible, T(MkT), Abstract, mkAbstract, C(method), hiddenToo) where

data T = MkT Int | Private Int

data Abstract = MkAbstract Int

mkAbstract :: Int -> Abstract
mkAbstract = MkAbstract

class C a where
  method :: a -> Int
  secretMethod :: a -> Int

instance C T where
  method _ = 1
  secretMethod _ = 2

visible :: Int
visible = 1

hiddenToo :: Int
hiddenToo = 2

notExported :: Int
notExported = 3
