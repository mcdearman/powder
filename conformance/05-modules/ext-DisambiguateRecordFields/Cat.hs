module Cat (Cat (..)) where

data Cat = Cat {name :: String, lives :: Int} deriving (Show)
