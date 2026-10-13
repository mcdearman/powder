{-# LANGUAGE GHC2021 #-}
-- Names of files, taken apart and put together, as the filepath package
-- has them for a system whose separator is a slash.
module System.FilePath
  ( FilePath,
    pathSeparator,
    pathSeparators,
    isPathSeparator,
    extSeparator,
    isExtSeparator,
    splitExtension,
    takeExtension,
    replaceExtension,
    (-<.>),
    dropExtension,
    addExtension,
    hasExtension,
    (<.>),
    splitExtensions,
    dropExtensions,
    takeExtensions,
    replaceExtensions,
    isExtensionOf,
    stripExtension,
    splitFileName,
    takeFileName,
    replaceFileName,
    dropFileName,
    takeBaseName,
    replaceBaseName,
    takeDirectory,
    replaceDirectory,
    combine,
    (</>),
    splitPath,
    joinPath,
    splitDirectories,
    hasTrailingPathSeparator,
    addTrailingPathSeparator,
    dropTrailingPathSeparator,
    isRelative,
    isAbsolute,
  )
where

import Data.List (isSuffixOf, stripPrefix)

infixr 7 <.>, -<.>

infixr 5 </>

pathSeparator :: Char
pathSeparator = '/'

pathSeparators :: [Char]
pathSeparators = "/"

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'

extSeparator :: Char
extSeparator = '.'

isExtSeparator :: Char -> Bool
isExtSeparator c = c == '.'

-- What is after the last dot of the file's name, with the dot.
splitExtension :: FilePath -> (String, String)
splitExtension path = case break isExtSeparator (reverse name) of
  (_, []) -> (path, "")
  (rest, dot : before) -> (directory ++ reverse before, dot : reverse rest)
  where
    (directory, name) = splitFileName' path

takeExtension :: FilePath -> String
takeExtension = snd . splitExtension

dropExtension :: FilePath -> FilePath
dropExtension = fst . splitExtension

replaceExtension :: FilePath -> String -> FilePath
replaceExtension path extension = dropExtension path <.> extension

(-<.>) :: FilePath -> String -> FilePath
(-<.>) = replaceExtension

addExtension :: FilePath -> String -> FilePath
addExtension path "" = path
addExtension path extension@(c : _)
  | isExtSeparator c = path ++ extension
  | otherwise = path ++ [extSeparator] ++ extension

(<.>) :: FilePath -> String -> FilePath
(<.>) = addExtension

hasExtension :: FilePath -> Bool
hasExtension = any isExtSeparator . takeFileName

-- Everything from the first dot of the file's name.
splitExtensions :: FilePath -> (FilePath, String)
splitExtensions path = (directory ++ before, extensions)
  where
    (directory, name) = splitFileName' path
    (before, extensions) = break isExtSeparator name

dropExtensions :: FilePath -> FilePath
dropExtensions = fst . splitExtensions

takeExtensions :: FilePath -> String
takeExtensions = snd . splitExtensions

replaceExtensions :: FilePath -> String -> FilePath
replaceExtensions path extension = dropExtensions path <.> extension

isExtensionOf :: String -> FilePath -> Bool
isExtensionOf extension@('.' : _) = isSuffixOf extension . takeExtensions
isExtensionOf extension = isSuffixOf ('.' : extension) . takeExtensions

stripExtension :: String -> FilePath -> Maybe FilePath
stripExtension [] path = Just path
stripExtension extension@(c : _) path = fmap reverse (stripPrefix (reverse dotted) (reverse path))
  where
    dotted = if isExtSeparator c then extension else '.' : extension

-- The directory, with its slash, and the file's name: the first is
-- nothing where there is no slash.
splitFileName' :: FilePath -> (String, String)
splitFileName' path = (reverse before, reverse name)
  where
    (name, before) = break isPathSeparator (reverse path)

splitFileName :: FilePath -> (String, String)
splitFileName path = (if null directory then "./" else directory, name)
  where
    (directory, name) = splitFileName' path

takeFileName :: FilePath -> FilePath
takeFileName = snd . splitFileName

dropFileName :: FilePath -> FilePath
dropFileName = fst . splitFileName

replaceFileName :: FilePath -> String -> FilePath
replaceFileName path name = fst (splitFileName' path) </> name

takeBaseName :: FilePath -> String
takeBaseName = dropExtension . takeFileName

replaceBaseName :: FilePath -> String -> FilePath
replaceBaseName path name = combineAlways directory (name <.> extension)
  where
    (directory, file) = splitFileName' path
    extension = takeExtension file

-- The directory a file is in: `.` where the name has no slash in it.
takeDirectory :: FilePath -> FilePath
takeDirectory path = dropTrailingPathSeparator (dropFileName path)

replaceDirectory :: FilePath -> String -> FilePath
replaceDirectory path directory = combineAlways directory (takeFileName path)

-- One name under another. A second that starts at the root is itself.
combine :: FilePath -> FilePath -> FilePath
combine a b
  | isAbsolute b = b
  | otherwise = combineAlways a b

combineAlways :: FilePath -> FilePath -> FilePath
combineAlways a b
  | null a = b
  | null b = a
  | isPathSeparator (last a) = a ++ b
  | otherwise = a ++ [pathSeparator] ++ b

(</>) :: FilePath -> FilePath -> FilePath
(</>) = combine

-- Each part with the slashes after it.
splitPath :: FilePath -> [FilePath]
splitPath [] = []
splitPath path = (part ++ slashes) : splitPath rest
  where
    (part, after) = break isPathSeparator path
    (slashes, rest) = span isPathSeparator after

splitDirectories :: FilePath -> [FilePath]
splitDirectories = map trimmed . splitPath
  where
    trimmed part
      -- The root is kept as it is written, however many slashes.
      | all isPathSeparator part = part
      | otherwise = takeWhile (not . isPathSeparator) part

joinPath :: [FilePath] -> FilePath
joinPath = foldr combine ""

hasTrailingPathSeparator :: FilePath -> Bool
hasTrailingPathSeparator "" = False
hasTrailingPathSeparator path = isPathSeparator (last path)

addTrailingPathSeparator :: FilePath -> FilePath
addTrailingPathSeparator path = if hasTrailingPathSeparator path then path else path ++ [pathSeparator]

-- Without the slashes at its end, unless it is the root.
dropTrailingPathSeparator :: FilePath -> FilePath
dropTrailingPathSeparator path
  | hasTrailingPathSeparator path && not (all isPathSeparator path) =
      let trimmed = reverse (dropWhile isPathSeparator (reverse path)) in if null trimmed then [last path] else trimmed
  | otherwise = path

isAbsolute :: FilePath -> Bool
isAbsolute ('/' : _) = True
isAbsolute _ = False

isRelative :: FilePath -> Bool
isRelative = not . isAbsolute
