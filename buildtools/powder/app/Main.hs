-- | @powder@: the build tool.
module Main (main) where

import Control.Concurrent (getNumCapabilities)
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as L
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Powder.Build
import Powder.Build.Graph
import Powder.Build.Manifest
import Powder.Build.Status
import qualified Powder.Bytecode as Bytecode
import qualified Powder.Core as Core
import Powder.Diagnostics (printFailure)
import Powder.Frontend (axcut)
import Powder.Package (entry)
import Powder.Stubs (stubs)
import Powder.SKI.Compile (compileProgram)
import Powder.SKI.Eval (Hooks (..), RuntimeError (..), run, stdHooks)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process (rawSystem)
import Text.Read (readMaybe)

usage :: String
usage =
  unlines
    [ "powder: build Haskell 2010 packages",
      "",
      "  powder init [PATH] [--name NAME]   start a package: a manifest and a Main.hs",
      "  powder build [PATH]                compile a package and the packages it depends on",
      "  powder run [PATH] [-- ARGS]        build a package, and run its Main",
      "  powder clean [PATH]                remove what builds of a package have kept",
      "",
      "  -j N, --jobs N       compile up to N packages at once (default: one a processor)",
      "  --runtime NAME       run on `vm`, the machine in Rust, or on `reducer`, the one",
      "                       built in (default: vm if powder-vm is found)",
      "",
      "A package is a directory with a Powder.toml:",
      "",
      "  [package]",
      "  name = \"app\"",
      "  version = \"0.1.0\"",
      "",
      "  edition = \"Powder2026\"            (the default; or Haskell2010, GHC2021, GHC2024)",
      "  extensions = [\"LambdaCase\"]       (turned on, or off with No, beside the edition)",
      "",
      "  [dependencies]",
      "  util = { path = \"../util\" }",
      "",
      "Its modules are under src/, A.B in src/A/B.hs, and src/Main.hs is where it starts."
    ]

data Flags = Flags
  { flagPath :: Maybe FilePath,
    flagJobs :: Maybe Int,
    flagName :: Maybe String,
    flagRuntime :: Maybe String,
    flagArguments :: [String]
  }

flags :: [String] -> Either String Flags
flags = go (Flags Nothing Nothing Nothing Nothing [])
  where
    go found = \case
      [] -> Right found
      "--" : rest -> Right found {flagArguments = rest}
      flag : n : rest | flag `elem` ["-j", "--jobs"] -> case readMaybe n of
        Just jobs | jobs > 0 -> go found {flagJobs = Just jobs} rest
        _ -> Left ("`" ++ flag ++ "` takes how many packages to compile at once, and `" ++ n ++ "` is not a number of them")
      "--name" : name : rest -> go found {flagName = Just name} rest
      "--runtime" : name : rest
        | name `elem` ["vm", "reducer"] -> go found {flagRuntime = Just name} rest
        | otherwise -> Left ("there is no runtime `" ++ name ++ "`: there are `vm` and `reducer`")
      flag : _ | "-" `isPrefixOf` flag -> Left ("there is no option `" ++ flag ++ "`")
      path : rest
        | Nothing <- flagPath found -> go found {flagPath = Just path} rest
        | otherwise -> Left ("one package at a time: `" ++ path ++ "` is a second")

main :: IO ()
main =
  getArgs >>= \case
    command : rest | command `elem` ["init", "build", "run", "clean"] -> case flags rest of
      Left message -> complain message >> exitWith (ExitFailure 2)
      Right found -> case command of
        "init" -> initialise found
        "build" -> void (building found)
        "run" -> building found >>= running found
        _ -> clean found
    [flag] | flag `elem` ["--help", "-h", "help"] -> putStr usage
    _ -> hPutStr stderr usage >> exitWith (ExitFailure 2)

-- | Write a manifest and a program that says hello.
initialise :: Flags -> IO ()
initialise found = do
  let dir = fromMaybe "." (flagPath found)
  createDirectoryIfMissing True dir
  root <- canonicalizePath dir
  let name = fromMaybe (takeFileName root) (flagName found)
  exists <- doesFileExist (root </> manifestFile)
  when exists (complain (root </> manifestFile ++ " is already there") >> exitFailure)
  unless (validName name) $ do
    complain ("`" ++ name ++ "` is not a package's name: a name is letters, digits, `-` and `_`, and starts with a letter. Give one with --name")
    exitFailure
  writeFile (root </> manifestFile) (unlines ["[package]", "name = " ++ show name, "version = \"0.1.0\"", "", "[dependencies]"])
  createDirectoryIfMissing True (root </> "src")
  hasMain <- doesFileExist (root </> "src" </> "Main.hs")
  unless hasMain (writeFile (root </> "src" </> "Main.hs") (unlines ["module Main (main) where", "", "main :: IO ()", "main = putStrLn \"Hello, world!\""]))
  ignored <- doesFileExist (root </> ".gitignore")
  unless ignored (writeFile (root </> ".gitignore") "/target/\n")
  status <- newStatus
  say status "Created" ("package `" ++ name ++ "` at " ++ root)

-- | Where what is kept between builds of anything is.
home :: IO FilePath
home = lookupEnv "POWDER_HOME" >>= maybe ((</> ".powder") <$> getHomeDirectory) pure

graphAt :: Flags -> IO Graph
graphAt found = discover (fromMaybe "." (flagPath found)) >>= either (\message -> complain message >> exitFailure) pure

-- | Build, and leave if anything is wrong. Gives the program and where
-- its image was written, if there is a program.
building :: Flags -> IO (Graph, Maybe (FilePath, Core.Program))
building found = do
  graph <- graphAt found
  status <- newStatus
  kept <- home
  processors <- getNumCapabilities
  built <- build (Options (fromMaybe processors (flagJobs found)) kept status) graph
  mapM_ (printFailure stderr) (builtFailures built)
  unless (null (builtFailures built)) $ do
    complain ("could not compile " ++ unwords ["`" ++ name ++ "`" | (name, Failed) <- builtSteps built])
    exitFailure
  image <- forM (builtProgram built) $ \program -> do
    let file = imageOf graph
    createDirectoryIfMissing True (takeDirectory file)
    L.writeFile file (Bytecode.image (axcut entry program))
    -- C for what the program exports, for C to link with.
    unless (null (Core.progExports program)) $ do
      let stub = takeDirectory (takeDirectory file) </> "stubs" </> graphRoot graph ++ "_stub.c"
      createDirectoryIfMissing True (takeDirectory stub)
      writeFile stub (stubs (Core.progExports program))
      note status "Exports" (show (length (Core.progExports program)) ++ " for C to call: " ++ stub)
    pure (file, program)
  let compiled = length [() | (_, Rebuilt) <- builtSteps built]
  say status "Finished" (show compiled ++ " compiled, " ++ show (length [() | (_, Fresh) <- builtSteps built]) ++ " fresh" ++ maybe "" ((" -> " ++) . fst) image)
  pure (graph, image)

-- | Run a built program: on the machine in Rust if there is one, and
-- otherwise by the reducer, from the same build.
running :: Flags -> (Graph, Maybe (FilePath, Core.Program)) -> IO ()
running found (graph, image) = case image of
  Nothing -> complain ("`" ++ graphRoot graph ++ "` has no src/Main.hs, so there is nothing to run") >> exitFailure
  Just (file, program) -> do
    status <- newStatus
    machine <- if flagRuntime found == Just "reducer" then pure Nothing else findMachine
    case machine of
      Just vm -> do
        say status "Running" (file ++ " on " ++ vm)
        rawSystem vm (file : flagArguments found) >>= exitWith
      Nothing
        | flagRuntime found == Just "vm" -> complain "powder-vm was not found: put it on the PATH, or say where it is with POWDER_VM" >> exitFailure
        | otherwise -> do
            say status "Running" (graphRoot graph ++ " on the reducer")
            void (run stdHooks {programName = graphRoot graph, arguments = flagArguments found, exported = Core.progExports program} (compileProgram program))
              `catch` \(RuntimeError message) -> hFlush stdout >> complain ("runtime error: " ++ message) >> exitFailure
  where
    findMachine =
      lookupEnv "POWDER_VM" >>= \case
        Just vm -> pure (Just vm)
        Nothing -> do
          beside <- (</> "powder-vm") . takeDirectory <$> getExecutablePath
          there <- doesFileExist beside
          if there then pure (Just beside) else findExecutable "powder-vm"

clean :: Flags -> IO ()
clean found = do
  root <- canonicalizePath (fromMaybe "." (flagPath found))
  status <- newStatus
  there <- doesDirectoryExist (root </> "target")
  if there
    then removeDirectoryRecursive (root </> "target") >> say status "Removed" (root </> "target")
    else note status "Clean" "nothing to remove"
