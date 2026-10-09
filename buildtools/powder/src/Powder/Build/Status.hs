-- | What a build says as it goes: a line a step, to the standard error,
-- with what is being done on the left.
module Powder.Build.Status (Status, newStatus, quiet, say, note, complain) where

import Control.Concurrent.MVar
import Control.Monad
import Data.Maybe (isNothing)
import System.Environment (lookupEnv)
import System.IO

-- | Where to say things, or nowhere. Lines from threads at work at once
-- are not mixed.
data Status = Status {statusLock :: MVar (), statusColour :: Bool, statusShown :: Bool}

newStatus :: IO Status
newStatus = do
  terminal <- hIsTerminalDevice stderr
  noColour <- lookupEnv "NO_COLOR"
  lock <- newMVar ()
  pure (Status lock (terminal && isNothing noColour) True)

-- | A status that says nothing.
quiet :: IO Status
quiet = (\lock -> Status lock False False) <$> newMVar ()

line :: Status -> String -> String -> String -> IO ()
line status colour verb rest =
  when (statusShown status) . withMVar (statusLock status) $ \() ->
    hPutStrLn stderr (replicate (12 - length verb) ' ' ++ painted ++ " " ++ rest)
  where
    painted = if statusColour status then "\ESC[1;" ++ colour ++ "m" ++ verb ++ "\ESC[0m" else verb

-- | A step taken: @Compiling@, @Finished@.
say :: Status -> String -> String -> IO ()
say status = line status "32"

-- | Something by the way.
note :: Status -> String -> String -> IO ()
note status = line status "36"

-- | Something wrong, which is always said.
complain :: String -> IO ()
complain message = hPutStrLn stderr ("error: " ++ message)
