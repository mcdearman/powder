-- Haskell 2010 Report 39 (System.Environment): getEnv (PATH is assumed to be set; a missing variable
-- raises an I/O error satisfying isDoesNotExistError) and getEnvironment (values are not printed)
module Main (main) where

import System.Environment
import System.IO.Error

missing :: String
missing = "POWDER_CONFORMANCE_SURELY_UNSET_VARIABLE_9431"

main :: IO ()
main = do
  path <- getEnv "PATH"
  print ("PATH is non-empty", not (null path))
  pathAgain <- getEnv "PATH"
  print ("getEnv is repeatable", pathAgain == path)
  r <- catchIOError (getEnv missing >>= \v -> return ("found " ++ v)) (\_ -> return "handler ran")
  print ("missing variable", r)
  k <- catchIOError (getEnv missing >> return False) (\e -> return (isDoesNotExistError e))
  print ("missing variable is a does-not-exist error", k)
  t <- tryIOError (getEnv missing)
  print ("tryIOError gives Left", either (const True) (const False) t)
  u <- catchIOError (getEnv missing >> return True) (\e -> return (isUserError e))
  print ("missing variable is not a user error", u)
  env <- getEnvironment
  print ("environment is a list of pairs", length env >= 0)
  print ("environment is non-empty", not (null env))
  print ("environment contains PATH", lookup "PATH" env == Just path)
  print ("environment lacks the missing variable", lookup missing env == Nothing)
  print ("variable names are non-empty", all (not . null . fst) env)
  print ("getEnv agrees with getEnvironment", fmap (not . null) (lookup "PATH" env))
