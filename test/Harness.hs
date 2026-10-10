-- | Running programs for tests: on each runtime, with what they are given
-- to read, and with what they wrote compared against a file.
module Harness
  ( Runtime (..),
    Outcome (..),
    execute,
    machineBuilt,
    goldenAt,
  )
where

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as L
import Data.IORef
import qualified Powder.AxCut.Machine as AxCut
import qualified Powder.Bytecode as Bytecode
import Powder.Core (Program (..))
import Powder.Frontend (axcut)
import Powder.SKI.Compile (compileProgram)
import Powder.SKI.Eval (Hooks (..), RuntimeError (..), run)
import System.Directory
import System.Exit
import System.FilePath
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
import Test.Hspec.Golden

-- | The three things a program can be run on. The first is the reference:
-- the others must do what it does.
data Runtime = Reducer | AxCutMachine | RustMachine
  deriving (Eq, Enum, Bounded)

instance Show Runtime where
  show = \case
    Reducer -> "on the combinator reducer"
    AxCutMachine -> "on the machine for AxCut"
    RustMachine -> "on the machine in Rust"

-- | What a program wrote, the status it left with, and what went wrong if
-- something did.
data Outcome = Outcome {written :: String, status :: Int, trouble :: String}

machine :: FilePath
machine = "runtime/target/release/powder-vm"

-- | Whether the machine in Rust has been built, without which it is not
-- tested.
machineBuilt :: IO Bool
machineBuilt = doesFileExist machine

-- | Run a program, named for where its image may be put, given what it is
-- to read. It is given the arguments @one@ and @two words@, and a minute.
execute :: Runtime -> String -> Program -> String -> IO Outcome
execute runtime name prog input = case runtime of
  Reducer -> inProcess (\hooks -> void (run hooks (compileProgram prog)))
  AxCutMachine -> inProcess (\hooks -> void (AxCut.run hooks (axcut "main" prog)))
  RustMachine -> do
    tmp <- getTemporaryDirectory
    let image = tmp </> map (\c -> if c `elem` "/ " then '_' else c) name <.> "pbc"
    L.writeFile image (Bytecode.image (axcut "main" prog))
    ran <- timeout limit (readProcessWithExitCode machine [image, "one", "two words"] input)
    removeFile image
    pure $ case ran of
      Nothing -> Outcome "" 124 "it did not finish"
      Just (ExitSuccess, out, err) -> Outcome out 0 err
      Just (ExitFailure n, out, err) -> Outcome out n err
  where
    limit = 60000000
    inProcess go = do
      out <- newIORef ""
      rest <- newIORef input
      let hooks =
            Hooks
              { putCh = \h c -> when (h == 1) (modifyIORef' out (c :)),
                getCh = atomicModifyIORef' rest (\s -> (drop 1 s, case s of c : _ -> Just c; [] -> Nothing)),
                programName = "powder",
                arguments = ["one", "two words"],
                exported = progExports prog
              }
      ended <- timeout limit (try (go hooks))
      text <- reverse <$> readIORef out
      pure $ case ended of
        Nothing -> Outcome text 124 "it did not finish"
        Just (Right ()) -> Outcome text 0 ""
        Just (Left e) -> case fromException e of
          Just ExitSuccess -> Outcome text 0 ""
          Just (ExitFailure n) -> Outcome text n ""
          Nothing -> case fromException e of
            Just (RuntimeError message) -> Outcome text 1 message
            Nothing -> Outcome text 1 (show e)

-- | Some text, to be the same as what a file holds. The file is written
-- if it is not there.
goldenAt :: FilePath -> String -> Golden String
goldenAt file text =
  Golden
    { output = text,
      encodePretty = id,
      writeToFile = \path s -> createDirectoryIfMissing True (takeDirectory path) >> writeFile path s,
      readFromFile = readFile,
      goldenFile = file,
      actualFile = Nothing,
      failFirstTime = False
    }
