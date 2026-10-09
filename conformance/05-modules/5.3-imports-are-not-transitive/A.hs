module A (revealed) where

import B
import Data.Char

revealed :: String
revealed = map toUpper secret ++ show (ord 'a')
