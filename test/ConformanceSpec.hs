-- | The conformance suite, in @conformance/@: programs written from the
-- Haskell 2010 Report, a chapter to a directory.
--
-- What each should write is in a file beside it, and is what GHC makes it
-- write under @-XHaskell2010@: @conformance/run.sh --write-expected@
-- makes those files, and needs GHC; this needs only them. Under
-- @reject/@ a test is a program that must be refused.
--
-- @conformance/KNOWN-FAILURES.txt@ lists the tests that are known not to
-- pass yet. One of those that fails is passed over; one that passes is a
-- failure here, so that the list is kept true.
module ConformanceSpec (spec) where

import Control.Monad
import Data.List (isPrefixOf, sort)
import Data.Maybe (fromMaybe)
import Harness
import Powder.Frontend
import System.Directory
import System.FilePath
import Test.Hspec
import Text.Read (readMaybe)

-- | A test: what it is called, the file to compile, and where what it
-- should write is kept.
data Test = Test {testName :: String, testPath :: FilePath, testMain :: FilePath, testExpected :: FilePath, testInput :: FilePath}

spec :: Spec
spec = do
  there <- runIO (doesDirectoryExist "conformance")
  when there $ do
    known <- runIO (listed "conformance/KNOWN-FAILURES.txt")
    chapters <- runIO (sort . filter (/= "reject") <$> directories "conformance")
    forM_ chapters $ \chapter -> describe chapter $ do
      tests <- runIO (testsIn ("conformance" </> chapter))
      parallel . forM_ tests $ \t -> it (testName t) (accepted known t)
    refused <- runIO (doesDirectoryExist "conformance/reject")
    when refused . describe "what must be refused" $ do
      rejects <- runIO (directories "conformance/reject")
      forM_ (sort rejects) $ \chapter -> describe chapter $ do
        tests <- runIO (testsIn ("conformance/reject" </> chapter))
        parallel . forM_ tests $ \t -> it (testName t) (rejected known t)

-- | The lines of a file that are not comments, if it is there.
listed :: FilePath -> IO [String]
listed file = do
  there <- doesFileExist file
  if there then filter (\l -> not (null l) && not ("#" `isPrefixOf` l)) . lines <$> readFile file else pure []

directories :: FilePath -> IO [FilePath]
directories dir = listDirectory dir >>= filterM (doesDirectoryExist . (dir </>))

-- | The tests of a chapter: under the modules chapter each is a
-- directory, with its modules in it, and elsewhere each is a file.
testsIn :: FilePath -> IO [Test]
testsIn dir = do
  entries <- sort <$> listDirectory dir
  fmap concat . forM entries $ \e -> do
    let path = dir </> e
    isDir <- doesDirectoryExist path
    hasMain <- doesFileExist (path </> "Main.hs")
    own <- doesFileExist (path </> "expected-output.txt")
    pure $ case () of
      _
        | isDir && hasMain -> [Test e path (path </> "Main.hs") (path </> (if own then "expected-output.txt" else "Main.out")) (path </> "Main.in")]
        | takeExtension e `elem` [".hs", ".lhs"] -> [Test e path path (path -<.> "out") (path -<.> "in")]
        | otherwise -> []

-- | The status a test's name says it leaves with: one ending @-exitN@
-- leaves with N.
wanted :: Test -> Int
wanted t = fromMaybe 0 (go (dropExtension (testName t)))
  where
    go s = case s of
      [] -> Nothing
      _ | "-exit" `isPrefixOf` s, Just n <- readMaybe (drop 5 s) -> Just n
      _ : rest -> go rest

accepted :: [String] -> Test -> Expectation
accepted known t = do
  expected <- doesFileExist (testExpected t)
  if not expected
    then pendingWith "GHC does not run this test as it stands, so there is nothing to compare with"
    else do
      want <- readFile (testExpected t)
      hasInput <- doesFileExist (testInput t)
      input <- if hasInput then readFile (testInput t) else pure ""
      loaded <- loadProgram (testMain t)
      passes <- case loaded of
        Left _ -> pure False
        Right prog -> do
          -- On the reference, and on the machine in Rust if it is built.
          built <- machineBuilt
          outcomes <- forM (Reducer : [RustMachine | built]) (\runtime -> execute runtime (testName t) prog input)
          pure (all (\o -> status o == wanted t && written o == want) outcomes)
      settle known t passes

rejected :: [String] -> Test -> Expectation
rejected known t = do
  loaded <- loadProgram (testMain t)
  settle known t (either (const True) (const False) loaded)

-- | A test that is known to fail and does is passed over; one that is
-- known to fail and does not must come off the list.
settle :: [String] -> Test -> Bool -> Expectation
settle known t passes
  | testPath t `elem` known = if passes then expectationFailure "this is listed in conformance/KNOWN-FAILURES.txt, and passes: take it off the list" else pendingWith "known not to pass yet"
  | otherwise = passes `shouldBe` True
