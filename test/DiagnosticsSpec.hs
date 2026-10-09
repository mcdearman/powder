-- | What is said of a program that is wrong: each file in @test/errors@
-- is one, and what the compiler says of it is kept beside it.
module DiagnosticsSpec (spec) where

import Control.Monad
import Data.List (sort)
import Harness
import Powder.Diagnostics (render)
import Powder.Frontend
import System.Directory
import System.FilePath
import Test.Hspec
import Test.Hspec.Golden ()

spec :: Spec
spec = do
  files <- runIO (sort . filter ((== ".hs") . takeExtension) <$> listDirectory "test/errors")
  forM_ files $ \f -> it f $ do
    said <- either render (const "nothing: it was accepted\n") <$> loadProgram ("test/errors" </> f)
    pure (goldenAt ("test/golden/errors" </> f -<.> "txt") said)
