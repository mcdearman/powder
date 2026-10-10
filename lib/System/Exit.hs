module System.Exit (ExitCode (..), exitWith, exitFailure, exitSuccess) where

import Powder.Base (leave)

data ExitCode = ExitSuccess | ExitFailure Int deriving (Eq, Ord, Show, Read)

exitWith :: ExitCode -> IO a
exitWith ExitSuccess = leave 0
exitWith (ExitFailure n) = leave (toInteger n)

exitFailure :: IO a
exitFailure = exitWith (ExitFailure 1)

exitSuccess :: IO a
exitSuccess = exitWith ExitSuccess
