{-# LANGUAGE GHC2021 #-}
-- System.FilePath: names of files taken apart and put together; and Data.Void, the type with no values
module Main (main) where

import Data.Void (Void, absurd, vacuous)
import System.FilePath

paths :: [FilePath]
paths =
  [ "",
    "file",
    "file.txt",
    "file.tar.gz",
    ".bashrc",
    "file.",
    "dir/file.hs",
    "dir.d/file",
    "/",
    "/file",
    "/usr/lib/libc.so.6",
    "a/b/c/",
    "a//b",
    "./x",
    "../up/down.x",
    "dir/.hidden",
    "dir/.hidden.txt",
    "trailing///",
    "//double/root",
    "a.b/c.d/e.f.g"
  ]

each :: Show a => String -> (FilePath -> a) -> IO ()
each name f = putStrLn (name ++ ": " ++ show (map f paths))

main :: IO ()
main = do
  print (pathSeparator, pathSeparators, isPathSeparator '/', isPathSeparator '\\', extSeparator, isExtSeparator '.')
  each "splitExtension" splitExtension
  each "takeExtension" takeExtension
  each "dropExtension" dropExtension
  each "hasExtension" hasExtension
  each "splitExtensions" splitExtensions
  each "takeExtensions" takeExtensions
  each "dropExtensions" dropExtensions
  each "splitFileName" splitFileName
  each "takeFileName" takeFileName
  each "dropFileName" dropFileName
  each "takeBaseName" takeBaseName
  each "takeDirectory" takeDirectory
  each "splitPath" splitPath
  each "splitDirectories" splitDirectories
  each "joinPath . splitPath" (joinPath . splitPath)
  each "hasTrailingPathSeparator" hasTrailingPathSeparator
  each "addTrailingPathSeparator" addTrailingPathSeparator
  each "dropTrailingPathSeparator" dropTrailingPathSeparator
  each "isAbsolute" isAbsolute
  each "isRelative" isRelative
  each "replaceExtension _ \"o\"" (`replaceExtension` "o")
  each "_ -<.> \".new\"" (-<.> ".new")
  each "_ <.> \"bak\"" (<.> "bak")
  each "addExtension _ \"\"" (`addExtension` "")
  each "replaceExtensions _ \"x\"" (`replaceExtensions` "x")
  each "replaceFileName _ \"other.c\"" (`replaceFileName` "other.c")
  each "replaceBaseName _ \"base\"" (`replaceBaseName` "base")
  each "replaceDirectory _ \"elsewhere\"" (`replaceDirectory` "elsewhere")
  each "\"root\" </> _" ("root" </>)
  each "_ </> \"leaf\"" (</> "leaf")
  each "_ </> \"/abs\"" (</> "/abs")
  each "isExtensionOf \"gz\"" (isExtensionOf "gz")
  each "isExtensionOf \".tar.gz\"" (isExtensionOf ".tar.gz")
  each "stripExtension \"hs\"" (stripExtension "hs")
  each "stripExtension \".tar.gz\"" (stripExtension ".tar.gz")
  print ("a" </> "b" </> "c" <.> "txt", "src" </> "Main" -<.> "o", joinPath ["/", "usr", "bin/", "ghc"], combine "x/" "y")
  print (map vacuous [[], []] :: [[Int]], fmap absurd (Nothing :: Maybe Void) :: Maybe Bool, either absurd (+ 1) (Right 41 :: Either Void Int))
