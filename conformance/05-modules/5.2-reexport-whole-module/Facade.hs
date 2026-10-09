module Facade (module Inner, module Facade, module Other) where

import Inner
import Other

facadeValue :: Int
facadeValue = innerValue + otherValue
