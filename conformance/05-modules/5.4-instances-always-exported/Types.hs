module Types (Thing(..)) where

data Thing = Thing Int | Other deriving (Show, Eq)
