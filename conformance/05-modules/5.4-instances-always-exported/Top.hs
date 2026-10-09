module Top (sample, describeAll) where

import Types
import Classes
import Instances ()

sample :: [Thing]
sample = [Thing 1, Other, Thing 2]

describeAll :: (String, String, String)
describeAll = (pretty sample, pretty (Thing 7), pretty [True, False])
