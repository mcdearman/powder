module System.Environment (getArgs, getProgName, getEnv, getEnvironment) where

import Powder.Base (IOError (..), IOErrorType (..))

-- Characters by their codes, asked for one at a time, up to the first -1.
spelled :: (Integer -> IO Integer) -> IO String
spelled at = go 0
  where
    go j = at j >>= \c -> if c < 0 then return [] else go (j + 1) >>= \rest -> return (primIntToChar c : rest)

-- An argument, or the program's name for -1.
argument :: Integer -> IO String
argument i = spelled (primArgChar i)

getArgs :: IO [String]
getArgs = primArgCount >>= \n -> mapM argument (enumFromTo 0 (n - 1))

getProgName :: IO String
getProgName = argument (negate 1)

-- The machine has each entry written NAME=value.
getEnvironment :: IO [(String, String)]
getEnvironment = go 0
  where
    go i =
      spelled (primEnvChar i) >>= \entry ->
        if null entry
          then return []
          else go (i + 1) >>= \rest -> return ((takeWhile (/= '=') entry, drop 1 (dropWhile (/= '=') entry)) : rest)

getEnv :: String -> IO String
getEnv name =
  getEnvironment >>= \entries -> case lookup name entries of
    Just value -> return value
    Nothing -> ioError (IOError NoSuchThing "getEnv" "no environment variable" Nothing (Just name))
