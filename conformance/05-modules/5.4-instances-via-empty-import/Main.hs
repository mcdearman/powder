-- Haskell 2010 Report 5.4 / 5.3: `import M ()` imports nothing by name but still brings M's instances into scope
module Main (main) where

import Colour (Colour(..), Pretty(..))
import ColourInstances ()
import qualified NumInstance ()
import Vec (Vec(..))

main :: IO ()
main = do
  print ("instance from empty import", pretty Red, pretty [Red, Blue])
  print ("Show instance from empty import", Blue, [Red, Blue])
  print ("Num instance from qualified empty import", Vec 1 2 + Vec 10 20, Vec 1 2 * 3)
  print ("fromInteger through instance", (5 :: Vec), negate (Vec 1 (-1)))
