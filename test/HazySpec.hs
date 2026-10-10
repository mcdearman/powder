-- | Hazy's tests, in @hazy/@: the ones that are Haskell 2010, kept as
-- Hazy has them. A program under @run/@ must write what is in the file
-- beside it; a module under @good/@ must be accepted, and one under
-- @bad/@ refused.
module HazySpec (spec) where

import Control.Monad
import Data.List (sort)
import Harness
import Powder.Diagnostics (Failure (..), render)
import Powder.Frontend
import System.Directory
import System.FilePath
import Test.Hspec

spec :: Spec
spec = do
  there <- runIO (doesDirectoryExist "hazy")
  when there $ do
    describe "programs" $ do
      names <- runIO (sort <$> listDirectory "hazy/run")
      parallel . forM_ names $ \name -> it name $ do
        want <- readFile ("hazy/run" </> name </> "result")
        loadProgramWith haskell2010 ("hazy/run" </> name </> "source" </> "Main.hs") >>= \case
          Left failure -> expectationFailure (render failure)
          Right prog -> do
            -- On the reference, and on the machine in Rust if it is built.
            built <- machineBuilt
            forM_ (Reducer : [RustMachine | built]) $ \runtime -> do
              outcome <- execute runtime name prog ""
              (show runtime, status outcome, written outcome) `shouldBe` (show runtime, 0, want)
    describe "what must be accepted" $ do
      files <- runIO (modulesIn "hazy/good")
      parallel . forM_ files $ \file -> it (takeFileName file) $ do
        refusal file >>= maybe (pure ()) expectationFailure
    describe "what must be refused" $ do
      groups <- runIO (sort <$> listDirectory "hazy/bad")
      forM_ groups $ \group -> describe group $ do
        files <- runIO (modulesIn ("hazy/bad" </> group))
        parallel . forM_ files $ \file -> it (takeFileName file) $ do
          refusal file >>= maybe (expectationFailure "this was accepted") (const (pure ()))

modulesIn :: FilePath -> IO [FilePath]
modulesIn dir = map (dir </>) . sort . filter ((== ".hs") . takeExtension) <$> listDirectory dir

-- | What is wrong with a module, if anything is.
refusal :: FilePath -> IO (Maybe String)
refusal file =
  loadTypedWith haskell2010 file >>= \case
    Left failure -> pure (Just (render failure))
    Right (Typed _ _ problems)
      | null (failureProblems problems) -> pure Nothing
      | otherwise -> pure (Just (render problems))
