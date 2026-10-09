module System.Exit (ExitCode (..), exitWith, exitFailure, exitSuccess) where

data ExitCode = ExitSuccess | ExitFailure Int deriving (Eq, Ord, Show)

exitWith :: ExitCode -> IO a
exitWith ExitSuccess = primExit 0
exitWith (ExitFailure n) = primExit (toInteger n)

exitFailure :: IO a
exitFailure = exitWith (ExitFailure 1)

exitSuccess :: IO a
exitSuccess = exitWith ExitSuccess
