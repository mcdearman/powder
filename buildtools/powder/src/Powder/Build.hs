-- | Building: compiling the packages of a graph, each after those it
-- depends on, and putting what comes of them together into a program.
--
-- The unit is the package. A package is compiled as one thing from all
-- its modules, given what was compiled of the packages below it and not
-- their sources; what comes of it is kept in a file, with a fingerprint
-- of everything that went into it. The next build compiles a package
-- again only if its fingerprint has changed: its own sources, the
-- compiler, or the fingerprint of a package it depends on. So a change
-- is paid for by the package it is in and by those above it, and by
-- nothing beside or below.
--
-- Packages that do not depend on each other are compiled at once, as
-- many at a time as there are jobs. The library the language comes with
-- is a package like any other, below all of them; it is kept with the
-- compiler and not with any one build, so it is compiled once for every
-- build there is.
module Powder.Build
  ( Options (..),
    Step (..),
    Built (..),
    build,
    base,
    targetOf,
    imageOf,
  )
where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Binary (decodeOrFail, encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.List (sort)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Version (showVersion)
import Paths_powder_build (version)
import Powder.Build.Fingerprint
import Powder.Build.Graph
import Powder.Build.Status
import qualified Powder.Core as Core
import Powder.Diagnostics (Failure (..), Problem (..))
import Powder.Frontend (Module, Unit, parseFile, readPrelude, unitModule)
import Powder.Package
import System.Directory
import System.Environment (getExecutablePath)
import System.FilePath
import System.IO.Error (catchIOError)

data Options = Options
  { -- | How many packages to compile at once.
    optionJobs :: Int,
    -- | Where what is kept between builds of anything is: the library the
    -- language comes with, compiled.
    optionHome :: FilePath,
    optionStatus :: Status
  }

-- | What was done about a package.
data Step
  = -- | Nothing: what was kept of it is still good.
    Fresh
  | Rebuilt
  | -- | It is wrong.
    Failed
  | -- | A package it depends on is wrong.
    Skipped
  deriving (Eq, Show)

-- | What a build came to: what was done about each package, in the
-- graph's order and after the language's own library, called @base@;
-- what is wrong; and the program, if nothing is and the package asked
-- for has a @Main@.
data Built = Built
  { builtSteps :: [(String, Step)],
    builtFailures :: [Failure],
    builtProgram :: Maybe Core.Program
  }

data Result = Result
  { resultFingerprint :: Fingerprint,
    resultStep :: Step,
    resultCompiled :: Maybe Compiled,
    resultFailure :: Maybe Failure
  }

-- | Where a build keeps what it makes: beside the package asked for.
targetOf :: Graph -> FilePath
targetOf graph = maybe "." packageRoot (packageNamed graph (graphRoot graph)) </> "target" </> "debug"

-- | Where a build's program is written, as an image for the machine.
imageOf :: Graph -> FilePath
imageOf graph = targetOf graph </> "bytecode" </> graphRoot graph <.> "pbc"

build :: Options -> Graph -> IO Built
build options graph = do
  compiler <- compilerId
  lib <- baseOf options compiler
  jobs <- newQSem (max 1 (optionJobs options))
  cells <- Map.fromList <$> mapM (\p -> (,) (packageName p) <$> newEmptyMVar) (graphPackages graph)
  let slot = slots graph
      resultOf name = maybe (pure (wrong ("there is no package " ++ name))) readMVar (Map.lookup name cells)
  forM_ (graphPackages graph) $ \package -> forkIO $ do
    -- Until everything it depends on is done there is nothing to do.
    direct <- mapM resultOf (packageDependencies package)
    result <-
      if not (all (isJust . resultCompiled) (lib : direct))
        then pure (Result 0 Skipped Nothing Nothing)
        else
          bracket_ (waitQSem jobs) (signalQSem jobs) $ do
            let below = closure graph package
                unseen = [p | p <- below, packageName p `notElem` packageDependencies package]
            rest <- mapM (resultOf . packageName) unseen
            one options graph compiler (Map.findWithDefault 1 (packageName package) slot) package (lib : direct) rest
              `catch` \(e :: SomeException) -> pure (wrong (packageName package ++ ": " ++ displayException e))
    forM_ (Map.lookup (packageName package) cells) (`putMVar` result)
  results <- mapM (resultOf . packageName) (graphPackages graph)
  let everything = lib : results
      runnable = maybe False (any ((== "Main") . fst) . packageModules) (packageNamed graph (graphRoot graph))
      linked = link (map compiledCode (mapMaybe resultCompiled everything))
      program
        | not (all (isJust . resultCompiled) everything) || not runnable = (Nothing, [])
        | "Main.main" `notElem` map fst (Core.progDefs linked) = (Nothing, [failed ("the module Main of `" ++ graphRoot graph ++ "` does not define main")])
        | otherwise = (Just (Core.shake entry linked), [])
  pure (Built (zip ("base" : map packageName (graphPackages graph)) (map resultStep everything)) (mapMaybe resultFailure everything ++ snd program) (fst program))

-- | The library the language comes with, compiled, from where it is kept
-- or afresh.
base :: Options -> IO (Either Failure Compiled)
base options = do
  result <- compilerId >>= baseOf options
  pure (maybe (Left (fromMaybe (failed "the library could not be compiled") (resultFailure result))) Right (resultCompiled result))

failed :: String -> Failure
failed message = Failure [] [Problem "error" message "" Nothing False]

wrong :: String -> Result
wrong message = Result 0 Failed Nothing (Just (failed message))

-- | What stands for the compiler: a package is compiled again by a
-- compiler that is not the one that compiled it.
compilerId :: IO String
compilerId = do
  exe <- getExecutablePath
  size <- getFileSize exe `catchIOError` \_ -> pure 0
  time <- (show <$> getModificationTime exe) `catchIOError` \_ -> pure ""
  pure (unwords [showVersion version, show size, time])

-- | Compile a package, or take what was kept of it. It is given what
-- came of the packages it may import from and of the rest below it, all
-- of which compiled.
one :: Options -> Graph -> String -> Int -> Package -> [Result] -> [Result] -> IO Result
one options graph compiler slot package seen unseen = do
  let root = packageName package == graphRoot graph
      modules = [m | m@(name, _) <- packageModules package, root || name /= "Main"]
      kept = targetOf graph </> "incremental" </> packageName package ++ (if root then "-bin" else "-lib") <.> "ppk"
  texts <- mapM (readFile . snd) modules
  mapM_ (evaluate . length) texts
  let print' =
        fingerprint
          ( [compiler, show slot, packageName package, packageRoot package, show root]
              ++ concat [[name, file, text] | ((name, file), text) <- zip modules texts]
              ++ map (shown . resultFingerprint) seen
          )
      interfaces = map compiledInterface . mapMaybe resultCompiled
  load kept print' >>= \case
    Just compiled -> pure (Result print' Fresh (Just compiled) Nothing)
    Nothing -> do
      say (optionStatus options) "Compiling" (packageName package ++ " v" ++ packageVersion package ++ " (" ++ packageRoot package ++ ")")
      case mapM (\((_, file), text) -> parseFile file text) (zip modules texts) >>= named modules >>= compile slot (interfaces seen) (interfaces unseen) of
        Left failure -> pure (Result print' Failed Nothing (Just failure))
        Right compiled -> do
          keep kept print' compiled
          pure (Result print' Rebuilt (Just compiled) Nothing)

-- | A module is the one its file is named for.
named :: [(String, FilePath)] -> [Unit Module] -> Either Failure [Unit Module]
named modules units = case [(name, file, unitModule u) | ((name, file), u) <- zip modules units, unitModule u /= name] of
  [] -> Right units
  (name, file, is) : _ -> Left (failed (file ++ " is where the module " ++ name ++ " goes, and the module in it is " ++ is))

-- | The library the language comes with, compiled: every module of it,
-- as the package below all others.
baseOf :: Options -> String -> IO Result
baseOf options compiler = do
  lib <- takeDirectory . fst <$> readPrelude
  files <- walk lib
  texts <- mapM readFile files
  mapM_ (evaluate . length) texts
  let print' = fingerprint (compiler : "base" : concat [[makeRelative lib file, text] | (file, text) <- zip files texts])
      kept = optionHome options </> "lib" </> "base-" ++ shown print' <.> "ppk"
  load kept print' >>= \case
    Just compiled -> pure (Result print' Fresh (Just compiled) Nothing)
    Nothing -> do
      say (optionStatus options) "Compiling" ("base (" ++ lib ++ ")")
      case zipWithM parseFile files texts >>= compile 0 [] [] of
        Left failure -> pure (Result print' Failed Nothing (Just failure))
        Right compiled -> do
          keep kept print' compiled
          pure (Result print' Rebuilt (Just compiled) Nothing)
  where
    walk dir = do
      entries <- sort <$> listDirectory dir
      fmap concat . forM entries $ \e -> do
        isDir <- doesDirectoryExist (dir </> e)
        if isDir then walk (dir </> e) else pure [dir </> e | takeExtension e == ".hs"]

-- What is kept ------------------------------------------------------------

-- | What says a file is a package this compiler kept.
magic :: String
magic = "powder package 1"

-- | What was kept in a file, if it is what a fingerprint asks for.
-- Anything else there is as good as nothing.
load :: FilePath -> Fingerprint -> IO (Maybe Compiled)
load file print' = do
  bytes <- (Just <$> B.readFile file) `catchIOError` \_ -> pure Nothing
  pure $ case decodeOrFail . L.fromStrict <$> bytes of
    Just (Right (_, _, (was, of', compiled))) | was == magic && of' == print' -> Just compiled
    _ -> Nothing

-- | Keep a compiled package. It is written beside where it goes and then
-- moved there, so that a build that is stopped leaves nothing half
-- written to be read.
keep :: FilePath -> Fingerprint -> Compiled -> IO ()
keep file print' compiled = do
  createDirectoryIfMissing True (takeDirectory file)
  me <- myThreadId
  let partial = file ++ "." ++ filter (`elem` ['0' .. '9']) (show me) ++ ".part"
  L.writeFile partial (encode (magic, print', compiled))
  renameFile partial file
