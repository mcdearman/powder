-- Haskell 2010 Report 5 (intro) / 5.7: a three-module import cycle (Rock -> Paper -> Scissors -> Rock) with a class defined
-- inside the cycle, instances in each module, qualified imports and a re-export along the cycle.
-- GHC only supports this with .hs-boot files, so the expected output (expected-output.txt) was derived by hand.
module Main (main) where

import Rock
import qualified Paper as P
import qualified Scissors as S

main :: IO ()
main = do
  print ("names", name Rock, name P.Paper, name S.Scissors)
  print ("beats", beats Rock, beats P.Paper, beats S.Scissors)
  print ("chain", chainFrom 4 Rock)
  print ("re-exported along the cycle", S.rockAgain == Rock, name S.rockAgain)
