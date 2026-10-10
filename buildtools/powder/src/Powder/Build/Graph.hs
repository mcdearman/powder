-- | The packages a build is of: the one asked for and every one it
-- depends on, each once, found by following the manifests.
--
-- A package is a directory. Its sources are under @src@ if it has one,
-- and otherwise the directory itself; the module @A.B@ is the file
-- @A/B.hs@ there, and @Main.hs@ is where a program starts. A package
-- with a @Main.hs@ can be run. When it is another package's dependency
-- its @Main@ is left out: a program has one.
module Powder.Build.Graph
  ( Package (..),
    Graph (..),
    discover,
    slots,
    packageNamed,
    closure,
  )
where

import Control.Monad
import Data.Char (isAlphaNum, isAsciiUpper)
import Data.IORef
import Data.List (intercalate, sort, sortOn)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Powder.Build.Fingerprint
import Powder.Build.Manifest
import Powder.Extensions (Settings)
import System.Directory
import System.FilePath

data Package = Package
  { packageName :: String,
    packageVersion :: String,
    -- | Where it is: a path with nothing in it to follow.
    packageRoot :: FilePath,
    -- | The packages its manifest names, in the manifest's order.
    packageDependencies :: [String],
    -- | Its modules, each with its file, in the order of their names.
    packageModules :: [(String, FilePath)],
    -- | The language its modules are written in.
    packageSettings :: Settings
  }
  deriving (Eq, Show)

-- | The packages of a build, each after those it depends on, and which
-- of them was asked for.
data Graph = Graph {graphRoot :: String, graphPackages :: [Package]}
  deriving (Eq, Show)

packageNamed :: Graph -> String -> Maybe Package
packageNamed graph name = lookup name [(packageName p, p) | p <- graphPackages graph]

-- | Every package one depends on, through however many others, in the
-- graph's order.
closure :: Graph -> Package -> [Package]
closure graph package = [p | p <- graphPackages graph, packageName p `elem` reached [] (packageDependencies package)]
  where
    reached seen = \case
      [] -> seen
      n : rest
        | n `elem` seen -> reached seen rest
        | otherwise -> reached (n : seen) (maybe [] packageDependencies (packageNamed graph n) ++ rest)

-- | The slot each package compiles in, which keeps what it makes apart
-- from what the others make. A package's slot comes of its name, so that
-- it is the same whatever else is built with it and what was compiled
-- can be kept; two names that come to one slot are moved apart, in the
-- order of the names. Slot 0 is the library the language comes with.
slots :: Graph -> Map.Map String Int
slots graph = fst (foldl place (Map.empty, []) (sort (map packageName (graphPackages graph))))
  where
    place (found, taken) name = let slot = free taken (1 + fromIntegral (fingerprint [name] `mod` 1000000007)) in (Map.insert name slot found, slot : taken)
    free taken slot = if slot `elem` taken then free taken (slot + 1) else slot :: Int

-- | Find the packages of a build, starting from a directory. What is
-- wrong is said as one message.
discover :: FilePath -> IO (Either String Graph)
discover start = do
  found <- newIORef Map.empty
  order <- newIORef []
  let visit trail dir = do
        root <- canonicalizePath dir
        exists <- doesDirectoryExist root
        known <- Map.lookup root <$> readIORef found
        case known of
          Just package -> pure (Right package)
          _
            | not exists -> pure (Left ("there is no package at " ++ dir))
            | (inside, (_, first) : _) <- break ((== root) . fst) trail ->
                pure (Left ("packages cannot depend on each other in a circle: " ++ intercalate " -> " (first : reverse (map snd inside) ++ [first])))
            | otherwise ->
                load root >>= \case
                  Left message -> pure (Left message)
                  Right (name, version, dependencies, settings) -> do
                    below <- forM dependencies $ \(Dependency key path) ->
                      doesDirectoryExist (root </> path) >>= \there ->
                        if not there
                          then pure (Left ("the dependency `" ++ key ++ "` of `" ++ name ++ "` is not at " ++ normalise (root </> path)))
                          else
                            visit ((root, name) : trail) (root </> path) >>= \case
                              Right package
                                | packageName package /= key ->
                                    pure (Left ("`" ++ name ++ "` calls its dependency at " ++ packageRoot package ++ " `" ++ key ++ "`, and that package is `" ++ packageName package ++ "`"))
                              other -> pure other
                    case sequence below of
                      Left message -> pure (Left message)
                      Right packages -> do
                        modules <- modulesOf root
                        case modules of
                          Left message -> pure (Left message)
                          Right ms -> do
                            let package = Package name version root (map packageName packages) ms settings
                            modifyIORef' found (Map.insert root package)
                            modifyIORef' order (package :)
                            pure (Right package)
  visit [] start >>= \case
    Left message -> pure (Left message)
    Right asked -> do
      packages <- reverse <$> readIORef order
      pure (clashes (Graph (packageName asked) packages))
  where
    load root = do
      let file = root </> manifestFile
      there <- doesFileExist file
      text <- if there then readFile file else pure ""
      case parseManifest text of
        Left message -> pure (Left (file ++ ": " ++ message))
        Right manifest -> do
          let name = fromMaybe (takeFileName (dropTrailingPathSeparator root)) (manifestName manifest)
          pure $
            if validName name
              then Right (name, fromMaybe "0.0.0" (manifestVersion manifest), manifestDependencies manifest, manifestSettings manifest)
              else Left (root ++ ": the package has no name in a manifest, and `" ++ name ++ "` is not one a package can have")

-- | No two packages of a build may have a module of one name: an import
-- names a module, and says nothing of which package.
clashes :: Graph -> Either String Graph
clashes graph = case [(m, map snd owners) | (m, owners) <- Map.toList byModule, length owners > 1] of
  [] -> Right graph
  (m, owners) : _ -> Left ("the module " ++ m ++ " is in more than one package: " ++ intercalate ", " (map (\p -> "`" ++ p ++ "`") owners))
  where
    byModule =
      Map.fromListWith
        (flip (++))
        [ (m, [(m, packageName p)])
          | p <- graphPackages graph,
            (m, _) <- packageModules p,
            m /= "Main" || packageName p == graphRoot graph
        ]

-- | A package's modules: the Haskell sources under where its sources
-- are, each named by where it is.
modulesOf :: FilePath -> IO (Either String [(String, FilePath)])
modulesOf root = do
  hasSrc <- doesDirectoryExist (root </> "src")
  let sources = if hasSrc then root </> "src" else root
  files <- walk sources
  let named = [(moduleOf (makeRelative sources f), f) | f <- files]
  pure $ case [f | (Nothing, f) <- named] of
    [] -> Right (sortOn fst [(m, f) | (Just m, f) <- named])
    bad -> Left ("a module's file is named for the module, `A/B.hs` for `A.B`, so these cannot be modules: " ++ intercalate ", " bad)
  where
    walk dir = do
      entries <- sort <$> listDirectory dir
      fmap concat . forM entries $ \e -> do
        let path = dir </> e
        isDir <- doesDirectoryExist path
        nested <- doesFileExist (path </> manifestFile)
        if isDir
          then if take 1 e == "." || e == "target" || nested then pure [] else walk path
          else pure [path | takeExtension e `elem` [".hs", ".lhs"]]
    moduleOf relative =
      let parts = splitDirectories (dropExtension relative)
       in if all part parts && not (null parts) then Just (intercalate "." parts) else Nothing
    part = \case
      c : rest -> isAsciiUpper c && all (\x -> isAlphaNum x || x `elem` "_'") rest
      [] -> False
