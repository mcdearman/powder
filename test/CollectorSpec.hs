-- | The Rust machine's memory: that values which refer to each other in a
-- circle are freed, which counting references does not do.
module CollectorSpec (spec) where

import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Harness
import Powder.Frontend
import System.Environment (setEnv)
import Test.Hspec

-- | The numbers the machine gives as it leaves, when asked: how many
-- values it made, freed by their counts, freed in circles, and left.
numbers :: String -> Maybe (Integer, Integer, Integer, Integer)
numbers said = case [map read (words (map (\c -> if isDigit c then c else ' ') l)) | l <- lines said, "gc: " `isPrefixOf` l] of
  [made, counted, circles, left, _] : _ -> Just (made, counted, circles, left)
  _ -> Nothing

spec :: Spec
spec = do
  built <- runIO machineBuilt
  let onTheMachine = if built then id else const (pendingWith "runtime/ has not been built")
  it "frees what refers to itself in a circle" . onTheMachine $ do
    setEnv "POWDER_GC_STATS" "1"
    Right prog <- loadProgram "examples/cycles.hs"
    outcome <- execute RustMachine "collector" prog ""
    status outcome `shouldBe` 0
    case numbers (trouble outcome) of
      Nothing -> expectationFailure ("the machine did not say how memory went: " ++ trouble outcome)
      Just (made, counted, circles, left) -> do
        -- Twenty thousand rings of three, and as many pairs of functions.
        circles `shouldSatisfy` (> 60000)
        made `shouldBe` counted + circles + left
        -- What is left is what the program still had when it ended.
        left `shouldSatisfy` (< 50000)
