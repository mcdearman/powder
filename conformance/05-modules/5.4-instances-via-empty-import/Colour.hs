module Colour (Colour(..), Pretty(..)) where

data Colour = Red | Blue

class Pretty a where
  pretty :: a -> String
