-- | The build tool: packages found, compiled apart, kept, and compiled
-- again only when they have to be.
module Main (main) where

import qualified ConformanceSpec
import Control.Monad
import Data.List (isInfixOf, nub, sort)
import qualified Data.Map as Map
import Powder.Build
import Powder.Build.Graph
import Powder.Build.Manifest
import Powder.Build.Status
import qualified Powder.Core as Core
import Powder.Diagnostics (render)
import Powder.Extensions (Edition (..), Extension (..), Settings (..), defaultSettings)
import Powder.SKI.Compile (compileProgram)
import Powder.SKI.Eval (Hooks (..), run)
import Data.IORef
import System.Directory
import System.FilePath
import Test.Hspec

-- | Write files under a directory, each a path and its lines.
files :: FilePath -> [(FilePath, [String])] -> IO ()
files root = mapM_ $ \(path, ls) -> do
  createDirectoryIfMissing True (takeDirectory (root </> path))
  writeFile (root </> path) (unlines ls)

manifest :: String -> [(String, FilePath)] -> [String]
manifest name dependencies =
  ["[package]", "name = " ++ show name, "version = \"0.1.0\"", "", "[dependencies]"]
    ++ [d ++ " = { path = " ++ show path ++ " }" | (d, path) <- dependencies]

-- | Three packages: app, which is a program, on text and util, and text
-- on util.
workspace :: FilePath -> IO ()
workspace root =
  files
    root
    [ ("util/Powder.toml", manifest "util" []),
      ("util/src/Util/Numbers.hs", ["module Util.Numbers (double, Pair (..), Sized (..)) where", "", "double :: Int -> Int", "double x = x + x", "", "data Pair = Pair Int Int deriving (Eq, Show)", "", "class Sized a where", "  size :: a -> Int", "  size _ = 1", "", "instance Sized Pair where", "  size (Pair a b) = a + b", "", "instance Sized Bool"]),
      ("text/Powder.toml", manifest "text" [("util", "../util")]),
      ("text/src/Text/Shout.hs", ["module Text.Shout (shout, twice) where", "", "import Data.Char (toUpper)", "import Util.Numbers (double)", "", "shout :: String -> String", "shout = map toUpper", "", "twice :: Int -> String", "twice n = show (double n)"]),
      ("app/Powder.toml", manifest "app" [("text", "../text"), ("util", "../util")]),
      ("app/src/Main.hs", ["module Main (main) where", "", "import App.Greeting (greeting)", "import Text.Shout (twice)", "import Util.Numbers", "", "instance Sized Int where", "  size n = n", "", "main :: IO ()", "main = do", "  putStrLn greeting", "  putStrLn (twice 21)", "  print (Pair 1 2, size (Pair 1 2), size True, size (7 :: Int))"]),
      ("app/src/App/Greeting.hs", ["module App.Greeting (greeting) where", "", "import Text.Shout (shout)", "", "greeting :: String", "greeting = shout \"hello\""])
    ]

-- | Build the package in a directory, giving what was done about each
-- package and what the program printed, or what was wrong.
building :: FilePath -> Int -> FilePath -> IO ([(String, Step)], Either String String)
building home jobs dir =
  discover dir >>= \case
    Left message -> pure ([], Left message)
    Right graph -> do
      status <- quiet
      built <- build (Options jobs home status) graph
      case (builtFailures built, builtProgram built) of
        ([], Just program) -> do
          out <- newIORef ""
          _ <- run (Hooks (\_ c -> modifyIORef' out (c :)) (pure Nothing) "test" [] (Core.progExports program)) (compileProgram program)
          (,) (builtSteps built) . Right . reverse <$> readIORef out
        ([], Nothing) -> pure (builtSteps built, Right "")
        (failures, _) -> pure (builtSteps built, Left (concatMap render failures))

rebuilt :: [(String, Step)] -> [String]
rebuilt steps = sort [name | (name, Rebuilt) <- steps, name /= "base"]

main :: IO ()
main = do
  scratch <- (</> "powder-buildtools-test") <$> getTemporaryDirectory
  gone <- doesDirectoryExist scratch
  when gone (removeDirectoryRecursive scratch)
  let home = scratch </> "home"
      fresh name = do
        let root = scratch </> name
        workspace root
        pure root
  status <- quiet
  lib <- base (Options 1 home status)
  hspec $ do
    either (it "the library compiles" . expectationFailure . render) (describe "the conformance suite, compiled as packages" . ConformanceSpec.spec) lib
    describe "the manifest" $ do
      it "has a name, a version and dependencies by path" $
        parseManifest (unlines (manifest "app" [("util", "../util")]) ++ "# said by the way\n")
          `shouldBe` Right (Manifest (Just "app") (Just "0.1.0") [Dependency "util" "../util"] defaultSettings)
      it "has an edition, and extensions turned on and off beside it" $
        fmap manifestSettings (parseManifest "[package]\nedition = \"Haskell2010\"\nextensions = [\"LambdaCase\", \"NoTupleSections\"]\n")
          `shouldBe` Right (Settings Haskell2010 [(LambdaCase, True), (TupleSections, False)])
      it "does not take an edition or an extension there is not" $ do
        parseManifest "[package]\nedition = \"Haskell2030\"\n" `shouldSatisfy` either ("no edition" `isInfixOf`) (const False)
        parseManifest "[package]\nextensions = [\"LambdaCases\"]\n" `shouldSatisfy` either ("no language extension" `isInfixOf`) (const False)
        parseManifest "[package]\nextensions = [\"GADTs\"]\n" `shouldSatisfy` either ("not supported yet" `isInfixOf`) (const False)
      it "says which line is wrong" $
        parseManifest "[package]\nname = app\n" `shouldSatisfy` either ("line 2" `isInfixOf`) (const False)
      it "does not take a dependency that is not a path" $
        parseManifest "[dependencies]\nutil = { git = \"https://example.com/util\" }\n" `shouldSatisfy` either ("path" `isInfixOf`) (const False)
      it "has names that are not packages' names refused" $
        parseManifest "[package]\nname = \"two words\"\n" `shouldSatisfy` either (const True) (const False)

    describe "the packages of a build" $ do
      it "are found from the one asked for, each after what it depends on" $ do
        root <- fresh "found"
        graph <- discover (root </> "app")
        fmap (map packageName . graphPackages) graph `shouldBe` Right ["util", "text", "app"]
        fmap graphRoot graph `shouldBe` Right "app"
      it "have modules named for their files" $ do
        root <- fresh "modules"
        graph <- discover (root </> "app")
        fmap (map (map fst . packageModules) . graphPackages) graph `shouldBe` Right [["Util.Numbers"], ["Text.Shout"], ["App.Greeting", "Main"]]
      it "cannot depend on each other in a circle" $ do
        root <- fresh "circle"
        writeFile (root </> "util" </> "Powder.toml") (unlines (manifest "util" [("app", "../app")]))
        discover (root </> "app") >>= (`shouldSatisfy` either ("app -> text -> util -> app" `isInfixOf`) (const False))
      it "must be where a manifest says" $ do
        root <- fresh "missing"
        removeDirectoryRecursive (root </> "util")
        discover (root </> "app") >>= (`shouldSatisfy` either ("is not at" `isInfixOf`) (const False))
      it "must be called what a manifest calls them" $ do
        root <- fresh "misnamed"
        writeFile (root </> "util" </> "Powder.toml") (unlines (manifest "tools" []))
        discover (root </> "app") >>= (`shouldSatisfy` either ("that package is `tools`" `isInfixOf`) (const False))
      it "cannot have a module of one name in two of them" $ do
        root <- fresh "clash"
        files root [("text/src/Util/Numbers.hs", ["module Util.Numbers where"])]
        discover (root </> "app") >>= (`shouldSatisfy` either ("Util.Numbers is in more than one package" `isInfixOf`) (const False))
      it "each have a slot of their own, the same whatever else is built" $ do
        root <- fresh "slots"
        Right whole <- discover (root </> "app")
        Right part <- discover (root </> "text")
        let all' = slots whole
        length (nub (Map.elems all')) `shouldBe` 3
        [Map.lookup n (slots part) | n <- ["util", "text"]] `shouldBe` [Map.lookup n all' | n <- ["util", "text"]]

    describe "a build" $ do
      it "compiles the packages apart and makes one program of them" $ do
        root <- fresh "program"
        (steps, out) <- building home 4 (root </> "app")
        out `shouldBe` Right "HELLO\n42\n(Pair 1 2,3,1,7)\n"
        rebuilt steps `shouldBe` ["app", "text", "util"]
      it "compiles nothing again when nothing has changed" $ do
        root <- fresh "unchanged"
        _ <- building home 4 (root </> "app")
        (steps, out) <- building home 4 (root </> "app")
        out `shouldBe` Right "HELLO\n42\n(Pair 1 2,3,1,7)\n"
        map snd steps `shouldBe` [Fresh, Fresh, Fresh, Fresh]
      it "compiles again only the package that changed and those above it" $ do
        root <- fresh "changed"
        _ <- building home 4 (root </> "app")
        appendFile (root </> "text" </> "src" </> "Text" </> "Shout.hs") "\n-- a change\n"
        (steps, _) <- building home 4 (root </> "app")
        rebuilt steps `shouldBe` ["app", "text"]
        appendFile (root </> "app" </> "src" </> "App" </> "Greeting.hs") "\n-- a change\n"
        (steps', _) <- building home 4 (root </> "app")
        rebuilt steps' `shouldBe` ["app"]
        appendFile (root </> "util" </> "src" </> "Util" </> "Numbers.hs") "\n-- a change\n"
        (steps'', out) <- building home 4 (root </> "app")
        rebuilt steps'' `shouldBe` ["app", "text", "util"]
        out `shouldBe` Right "HELLO\n42\n(Pair 1 2,3,1,7)\n"
      it "comes to the same whether packages are compiled one at a time or several" $ do
        one <- fresh "one-job"
        many <- fresh "many-jobs"
        (_, a) <- building home 1 (one </> "app")
        (_, b) <- building home 8 (many </> "app")
        a `shouldBe` b
      it "sees a change in a package below through the packages between" $ do
        root <- fresh "through"
        _ <- building home 4 (root </> "app")
        files root [("util/src/Util/Numbers.hs", ["module Util.Numbers (double, Pair (..), Sized (..)) where", "", "double :: Int -> Int", "double x = x * 3", "", "data Pair = Pair Int Int deriving (Eq, Show)", "", "class Sized a where", "  size :: a -> Int", "  size _ = 1", "", "instance Sized Pair where", "  size (Pair a b) = a + b", "", "instance Sized Bool"])]
        (_, out) <- building home 4 (root </> "app")
        out `shouldBe` Right "HELLO\n63\n(Pair 1 2,3,1,7)\n"
      it "reports what is wrong with a package, and does not compile those above it" $ do
        root <- fresh "wrong"
        files root [("text/src/Text/Shout.hs", ["module Text.Shout (shout, twice) where", "", "shout :: String -> String", "shout = not", "", "twice :: Int -> String", "twice = show"])]
        (steps, out) <- building home 4 (root </> "app")
        out `shouldSatisfy` either ("Shout.hs" `isInfixOf`) (const False)
        [step | (name, step) <- steps, name `elem` ["util", "text", "app"]] `shouldBe` [Rebuilt, Failed, Skipped]
      it "lets a package import only from what it depends on" $ do
        root <- fresh "hidden"
        writeFile (root </> "app" </> "Powder.toml") (unlines (manifest "app" [("text", "../text")]))
        (_, out) <- building home 4 (root </> "app")
        out `shouldSatisfy` either ("there is no module Util.Numbers" `isInfixOf`) (const False)
      it "builds a package with no Main, and has no program of it" $ do
        root <- fresh "library"
        (steps, out) <- building home 4 (root </> "text")
        out `shouldBe` Right ""
        rebuilt steps `shouldBe` ["text", "util"]
      it "leaves a dependency's Main out" $ do
        root <- fresh "two-mains"
        files root [("util/src/Main.hs", ["module Main (main) where", "", "main :: IO ()", "main = putStrLn \"util's own\""])]
        (_, out) <- building home 4 (root </> "app")
        out `shouldBe` Right "HELLO\n42\n(Pair 1 2,3,1,7)\n"
        (_, own) <- building home 4 (root </> "util")
        own `shouldBe` Right "util's own\n"
      it "needs a module to be in the file named for it" $ do
        root <- fresh "misplaced"
        files root [("text/src/Text/Shout.hs", ["module Text.Loud (shout) where", "", "shout :: String -> String", "shout = id"])]
        (_, out) <- building home 4 (root </> "app")
        out `shouldSatisfy` either ("the module in it is Text.Loud" `isInfixOf`) (const False)
