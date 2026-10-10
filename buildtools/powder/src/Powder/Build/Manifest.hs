-- | A package's manifest, @Powder.toml@:
--
-- > [package]
-- > name = "app"
-- > version = "0.1.0"
-- >
-- > [dependencies]
-- > util = { path = "../util" }
--
-- It is read as the part of TOML that takes: sections, keys with a string
-- or a table of strings on one line, and comments.
module Powder.Build.Manifest
  ( Manifest (..),
    Dependency (..),
    manifestFile,
    parseManifest,
    validName,
  )
where

import Data.Char (isAlphaNum, isAsciiLower, isAsciiUpper, isSpace)
import Data.List (dropWhileEnd)

manifestFile :: FilePath
manifestFile = "Powder.toml"

data Manifest = Manifest
  { manifestName :: Maybe String,
    manifestVersion :: Maybe String,
    manifestDependencies :: [Dependency]
  }
  deriving (Eq, Show)

-- | A package this one depends on: what the manifest calls it, and where
-- it is, from the manifest's directory.
data Dependency = Dependency {dependencyName :: String, dependencyPath :: FilePath}
  deriving (Eq, Show)

-- | A package's name: letters, digits, @-@ and @_@, starting with a
-- letter.
validName :: String -> Bool
validName = \case
  c : rest -> (isAsciiLower c || isAsciiUpper c) && all (\x -> isAlphaNum x && x < '\128' || x `elem` "-_") rest
  [] -> False

data Value = Text String | Table [(String, Value)]

-- | Read a manifest, or say what is wrong with it and on which line.
parseManifest :: String -> Either String Manifest
parseManifest text = go "" (Manifest Nothing Nothing []) (zip [1 :: Int ..] (lines text))
  where
    go _ manifest [] = Right manifest {manifestDependencies = reverse (manifestDependencies manifest)}
    go section manifest ((n, raw) : rest) = case trim (uncommented raw) of
      "" -> go section manifest rest
      '[' : header
        | last header == ']' -> go (trim (init header)) manifest rest
        | otherwise -> wrong n "a section's name ends with `]`"
      line -> case break (== '=') line of
        (key, '=' : value) -> do
          v <- maybe (wrong n ("`" ++ trim value ++ "` is not a string or a table of strings")) Right (parseValue (trim value))
          entry n section (unquoted (trim key)) v manifest >>= \m -> go section m rest
        _ -> wrong n "expected `key = value`"

    entry n section key value manifest = case (section, key, value) of
      ("package", "name", Text s)
        | validName s -> Right manifest {manifestName = Just s}
        | otherwise -> wrong n ("`" ++ s ++ "` is not a package's name: a name is letters, digits, `-` and `_`, and starts with a letter")
      ("package", "version", Text s) -> Right manifest {manifestVersion = Just s}
      ("dependencies", _, Table fields)
        | Just (Text path) <- lookup "path" fields -> Right manifest {manifestDependencies = Dependency key path : manifestDependencies manifest}
        | any ((`elem` ["git", "version"]) . fst) fields -> wrong n ("`" ++ key ++ "` is not given by a path: only `{ path = \"...\" }` is a dependency yet")
        | otherwise -> wrong n ("the dependency `" ++ key ++ "` needs a path: `" ++ key ++ " = { path = \"...\" }`")
      ("dependencies", _, Text _) -> wrong n ("the dependency `" ++ key ++ "` needs a path: `" ++ key ++ " = { path = \"...\" }`")
      -- What is not known is left for a later version to mean something by.
      _ -> Right manifest

    wrong n message = Left ("line " ++ show n ++ ": " ++ message)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

unquoted :: String -> String
unquoted s = case s of
  '"' : rest | not (null rest), last rest == '"' -> init rest
  _ -> s

-- | A line without its comment, which a @#@ outside a string starts.
uncommented :: String -> String
uncommented = go False
  where
    go _ [] = []
    go True ('\\' : c : rest) = '\\' : c : go True rest
    go inside ('"' : rest) = '"' : go (not inside) rest
    go False ('#' : _) = []
    go inside (c : rest) = c : go inside rest

parseValue :: String -> Maybe Value
parseValue s = case s of
  '"' : _ -> Text <$> string s
  '{' : rest | not (null rest), last rest == '}' -> Table <$> mapM field (filter (not . null) (map trim (commas (init rest))))
  _ -> Nothing
  where
    field f = case break (== '=') f of
      (key, '=' : value) -> (,) (unquoted (trim key)) <$> parseValue (trim value)
      _ -> Nothing
    string = \case
      '"' : rest -> case closing rest of
        (body, "\"") -> Just body
        _ -> Nothing
      _ -> Nothing
    -- The text of a string up to the quote that ends it, and what is left.
    closing = \case
      '\\' : c : rest -> let (body, end) = closing rest in (c : body, end)
      "\"" -> ("", "\"")
      c : rest | c /= '"' -> let (body, end) = closing rest in (c : body, end)
      other -> ("", "!" ++ other)
    -- Split at the commas that are not in a string.
    commas = go False ""
      where
        go _ acc [] = [reverse acc]
        go inside acc ('"' : rest) = go (not inside) ('"' : acc) rest
        go False acc (',' : rest) = reverse acc : go False "" rest
        go inside acc (c : rest) = go inside (c : acc) rest
