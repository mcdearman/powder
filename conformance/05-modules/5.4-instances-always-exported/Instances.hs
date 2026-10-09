-- exports nothing by name: only its instances
module Instances () where

import Types
import Classes

instance Pretty Thing where
  pretty (Thing n) = "<" ++ show n ++ ">"
  pretty Other = "<?>"

instance Pretty a => Pretty [a] where
  pretty = prettyList

instance Pretty Bool where
  pretty True = "yes"
  pretty False = "no"
