module DoubleDefault (inDoubleModule) where

default (Double)

inDoubleModule :: (String, String)
inDoubleModule = (show (2 ^ (10 :: Int)), show (1 + 2))
