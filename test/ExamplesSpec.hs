-- | The examples: each run on every runtime, and what it writes compared
-- with what GHC makes it write, which is beside it in a file; and the
-- types found for its definitions.
module ExamplesSpec (spec) where

import Control.Monad
import Data.List (sort)
import Harness
import Powder.Diagnostics (Failure (..), render)
import Powder.Frontend
import qualified Powder.HIR as HIR
import Powder.Typecheck (schemeOf)
import Powder.Types (prettyScheme)
import System.Directory
import System.FilePath
import Test.Hspec
import Test.Hspec.Golden ()

spec :: Spec
spec = do
  files <- runIO (sort . filter ((== ".hs") . takeExtension) <$> listDirectory "examples")
  built <- runIO machineBuilt
  forM_ files $ \f -> describe f $ do
    let path = "examples" </> f
    hasInput <- runIO (doesFileExist (path -<.> "in"))
    input <- runIO (if hasInput then readFile (path -<.> "in") else pure "")
    loaded <- runIO (loadProgram path)
    forM_ [minBound .. maxBound] $ \runtime -> case loaded of
      Left failure -> it (show runtime) (expectationFailure (render failure))
      Right prog
        | runtime == RustMachine && not built -> it (show runtime) (pendingWith "runtime/ has not been built")
        | otherwise -> it (show runtime) $ do
            outcome <- execute runtime f prog input
            -- A program that does not leave as it should has not written
            -- what it should.
            let ending = if status outcome == 0 then "" else "\n[left with status " ++ show (status outcome) ++ ": " ++ trouble outcome ++ "]\n"
            pure (goldenAt (path -<.> "out") (written outcome ++ ending))
    it "has the types it had" $ do
      checked <- loadTyped path
      pure . goldenAt ("test/golden/types" </> f -<.> "txt") $ case checked of
        Left failure -> render failure
        Right (Typed hir found problems) ->
          unlines
            ( [ HIR.nameText info ++ " :: " ++ prettyScheme hir scheme
                | b <- HIR.programBindings hir,
                  fmap HIR.originFile (HIR.origin hir (HIR.bindingId b)) == Just path,
                  x <- HIR.bound b,
                  Just info <- [HIR.nameInfo hir x],
                  Just scheme <- [schemeOf found x]
              ]
                ++ [render problems | not (null (failureProblems problems))]
            )
