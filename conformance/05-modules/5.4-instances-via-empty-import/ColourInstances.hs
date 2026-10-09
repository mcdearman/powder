module ColourInstances () where

import Colour

instance Show Colour where
  showsPrec _ Red = showString "RED"
  showsPrec _ Blue = showString "BLUE"

instance Pretty Colour where
  pretty Red = "r"
  pretty Blue = "b"

instance Pretty a => Pretty [a] where
  pretty = concatMap pretty
