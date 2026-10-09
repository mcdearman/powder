-- | The files a running program has open, for the machines written in
-- Haskell: the combinator reducer and the AxCut machine do the same things
-- to the world outside, and this is where they do them.
--
-- A file is known to the program by a number, from 3 up; 0, 1 and 2 are
-- the standard input, output and error, which a machine reaches through
-- its hooks and this does not. Nothing here raises: what cannot be done
-- gives a number that says so, and the library the program is linked with
-- makes an error of it.
module Powder.Files
  ( Files,
    newFiles,
    spell,
    open,
    close,
    get,
    put,
    atEnd,
    seek,
    resize,
    flush,
    terminal,
    environment,
  )
where

import Control.Exception
import Data.IORef
import qualified Data.Map as Map
import System.Environment (getEnvironment)
import System.IO
import System.IO.Error

data Files = Files
  { -- | The path being spelled out, backwards.
    path :: IORef String,
    files :: IORef (Map.Map Integer Handle),
    -- | How many files have been opened: a number is not used twice.
    count :: IORef Integer
  }

newFiles :: IO Files
newFiles = Files <$> newIORef "" <*> newIORef Map.empty <*> newIORef 0

-- | What to give when the world refuses.
refused :: a -> IO a -> IO a
refused instead action = action `catch` \(_ :: IOException) -> pure instead

file :: Files -> Integer -> IO (Maybe Handle)
file fs h = Map.lookup h <$> readIORef (files fs)

-- | Add a character to the path that 'open' will open.
spell :: Files -> Char -> IO ()
spell fs c = modifyIORef' (path fs) (c :)

-- | Open the path spelled out, to read (0), write (1), add to (2) or read
-- and write (3). Gives the file's number; or -1 where there is no such
-- file, -2 where it may not be opened, -3 where it is in use, and -4 for
-- anything else.
open :: Files -> Integer -> IO Integer
open fs mode = do
  name <- reverse <$> readIORef (path fs)
  writeIORef (path fs) ""
  opened <- try (openFile name (case mode of 0 -> ReadMode; 1 -> WriteMode; 2 -> AppendMode; _ -> ReadWriteMode))
  case opened of
    Left e
      | isDoesNotExistError e -> pure (-1)
      | isPermissionError e -> pure (-2)
      | isAlreadyInUseError e -> pure (-3)
      | otherwise -> pure (-4)
    Right handle -> do
      n <- (+ 3) <$> readIORef (count fs)
      modifyIORef' (count fs) (+ 1)
      modifyIORef' (files fs) (Map.insert n handle)
      pure n

close :: Files -> Integer -> IO ()
close fs h = do
  mapM_ (refused () . hClose) =<< file fs h
  modifyIORef' (files fs) (Map.delete h)

-- | The next character of a file, or nothing at its end.
get :: Files -> Integer -> IO (Maybe Char)
get fs h =
  file fs h >>= \case
    Just handle -> refused Nothing (hIsEOF handle >>= \done -> if done then pure Nothing else Just <$> hGetChar handle)
    Nothing -> pure Nothing

-- | Write a character to a file. False where the number is not a file's.
put :: Files -> Integer -> Char -> IO Bool
put fs h c =
  file fs h >>= \case
    Just handle -> True <$ refused () (hPutChar handle c)
    Nothing -> pure False

-- | Whether a file has nothing more to read. Nothing where the number is
-- not a file's.
atEnd :: Files -> Integer -> IO (Maybe Bool)
atEnd fs h = mapM (refused True . hIsEOF) =<< file fs h

-- | Move to a place in a file, counted from its start (0), from where it
-- is (1) or from its end (2), and give the place, counted from the start;
-- or, for 3, give the file's size and move nowhere. -1 where it cannot be
-- done.
seek :: Files -> Integer -> Integer -> Integer -> IO Integer
seek fs h whence offset =
  file fs h >>= \case
    Just handle
      | whence == 3 -> refused (-1) (hFileSize handle)
      | otherwise -> refused (-1) $ do
          hSeek handle (case whence of 0 -> AbsoluteSeek; 1 -> RelativeSeek; _ -> SeekFromEnd) offset
          hTell handle
    Nothing -> pure (-1)

-- | Make a file a size: 0 when done, and -1 when not.
resize :: Files -> Integer -> Integer -> IO Integer
resize fs h size =
  file fs h >>= \case
    Just handle -> refused (-1) (0 <$ hSetFileSize handle size)
    Nothing -> pure (-1)

-- | Send on what has been written and is being held.
flush :: Files -> Integer -> IO ()
flush fs h =
  file fs h >>= \case
    Just handle -> refused () (hFlush handle)
    Nothing -> refused () (hFlush (if h == 2 then stderr else stdout))

-- | Whether a file, or one of the standard three, is a terminal.
terminal :: Files -> Integer -> IO Bool
terminal fs h =
  file fs h >>= \case
    Just handle -> refused False (hIsTerminalDevice handle)
    Nothing -> refused False (hIsTerminalDevice (case h of 0 -> stdin; 1 -> stdout; _ -> stderr))

-- | A character of the environment: of its entry at a place, written
-- @NAME=value@, the character at a place. Nothing past the end of either.
environment :: Integer -> Integer -> IO (Maybe Char)
environment i j = do
  entries <- getEnvironment
  pure $ case drop (fromInteger i) entries of
    (name, value) : _ | i >= 0 -> lookup j (zip [0 ..] (name ++ '=' : value))
    _ -> Nothing
