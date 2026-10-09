-- | The conformance suite, compiled the way the build tool compiles: each
-- test as a package of its own, after the library the language comes
-- with and without that library's sources. It is the same tests as the
-- compiler's own suite runs with everything compiled together, so that
-- compiling apart is known to come to the same thing.
module ConformanceSpec (spec) where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.List (isPrefixOf, sort)
import Data.Maybe (fromMaybe)
import qualified Powder.Core as Core
import Powder.Frontend (parseFile)
import Powder.Package
import Powder.SKI.Compile (compileProgram)
import Powder.SKI.Eval (Hooks (..), RuntimeError (..), run)
import System.Directory
import System.Exit
import System.FilePath
import System.Timeout (timeout)
import Test.Hspec
import Text.Read (readMaybe)

-- | Where the suite is, from where the tests are run.
suite :: IO (Maybe FilePath)
suite = do
  found <- filterM doesDirectoryExist ["conformance", "../../conformance"]
  pure (case found of dir : _ -> Just dir; [] -> Nothing)

spec :: Compiled -> Spec
spec lib = do
  found <- runIO suite
  forM_ found $ \root -> do
    chapters <- runIO (sort . filter (/= "reject") <$> directories root)
    forM_ chapters $ \chapter -> describe chapter $ do
      tests <- runIO (testsIn (root </> chapter))
      parallel . forM_ tests $ \(name, sources, expected, input) -> it name $ do
        there <- doesFileExist expected
        if not there
          then pendingWith "there is nothing to compare with"
          else do
            want <- readFile expected
            hasInput <- doesFileExist input
            given <- if hasInput then readFile input else pure ""
            outcome <- compiled lib sources >>= either (const (pure Nothing)) (fmap Just . running name given)
            outcome `shouldBe` Just (wanted name, want)
    describe "what must be refused" $ do
      rejects <- runIO (sort <$> directories (root </> "reject"))
      forM_ rejects $ \chapter -> describe chapter $ do
        tests <- runIO (testsIn (root </> "reject" </> chapter))
        parallel . forM_ tests $ \(name, sources, _, _) -> it name $ do
          outcome <- compiled lib sources
          either (const True) (const False) outcome `shouldBe` True

directories :: FilePath -> IO [FilePath]
directories dir = listDirectory dir >>= filterM (doesDirectoryExist . (dir </>))

-- | The tests of a chapter, each with its sources: a file, or the
-- Haskell files of a directory.
testsIn :: FilePath -> IO [(String, [FilePath], FilePath, FilePath)]
testsIn dir = do
  entries <- sort <$> listDirectory dir
  fmap concat . forM entries $ \e -> do
    let path = dir </> e
    isDir <- doesDirectoryExist path
    hasMain <- doesFileExist (path </> "Main.hs")
    own <- doesFileExist (path </> "expected-output.txt")
    if isDir && hasMain
      then do
        sources <- walk path
        pure [(e, sources, path </> (if own then "expected-output.txt" else "Main.out"), path </> "Main.in")]
      else pure [(e, [path], path -<.> "out", path -<.> "in") | takeExtension e `elem` [".hs", ".lhs"]]
  where
    walk d = do
      entries <- sort <$> listDirectory d
      fmap concat . forM entries $ \e -> do
        isDir <- doesDirectoryExist (d </> e)
        if isDir then walk (d </> e) else pure [d </> e | takeExtension e `elem` [".hs", ".lhs"]]

-- | A test's program: its modules as a package after the library, and
-- the two put together.
compiled :: Compiled -> [FilePath] -> IO (Either String Core.Program)
compiled lib sources = do
  texts <- mapM readFile sources
  pure $ case zipWithM parseFile sources texts >>= compile 1 [compiledInterface lib] [] of
    Left _ -> Left "refused"
    Right package ->
      let linked = link [compiledCode lib, compiledCode package]
       in if "Main.main" `elem` map fst (Core.progDefs linked) then Right (Core.shake entry linked) else Left "no main"

-- | The status a test's name says it leaves with.
wanted :: String -> Int
wanted name = fromMaybe 0 (go (dropExtension name))
  where
    go s = case s of
      [] -> Nothing
      _ | "-exit" `isPrefixOf` s, Just n <- readMaybe (drop 5 s) -> Just n
      _ : rest -> go rest

-- | Run a program on the reducer: the status it leaves with, and what it
-- wrote to the standard output.
running :: String -> String -> Core.Program -> IO (Int, String)
running name input program = do
  out <- newIORef ""
  left <- newIORef input
  let hooks =
        Hooks
          { putCh = \h c -> when (h == 1) (modifyIORef' out (c :)),
            getCh = atomicModifyIORef' left (\case [] -> ([], Nothing); c : rest -> (rest, Just c)),
            programName = name,
            arguments = ["one", "two words"]
          }
  status <-
    timeout 60000000 ((0 <$ run hooks (compileProgram program)) `catches` [Handler (\e -> pure (case e of ExitSuccess -> 0; ExitFailure n -> n)), Handler (\(RuntimeError _) -> pure 1)])
  (,) (fromMaybe 124 status) . reverse <$> readIORef out
