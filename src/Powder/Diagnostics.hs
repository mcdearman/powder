-- | What is wrong with a program, and showing it against the source.
module Powder.Diagnostics
  ( Problem (..),
    Failure (..),
    summary,
    render,
    printFailure,
  )
where

import Data.Maybe (isJust, isNothing)
import Error.Diagnose
import Powder.Syntax.Token (Pos (..))
import Prettyprinter (unAnnotate)
import System.Environment (lookupEnv)
import System.IO (Handle, hIsTerminalDevice, hPutStr)

data Problem = Problem
  { -- | What kind of thing is wrong: @syntax error@, say.
    problemTitle :: String,
    problemMessage :: String,
    -- | The file it is in, as the 'Failure' names it.
    problemFile :: FilePath,
    -- | Where it starts and ends, if it is anywhere in particular.
    problemSpan :: Maybe (Pos, Pos),
    -- | Whether it is that the input ran out, so that more of it might
    -- have been right.
    problemAtEnd :: Bool
  }

-- | Problems, and the text of the files they are in.
data Failure = Failure
  { failureSources :: [(FilePath, String)],
    failureProblems :: [Problem]
  }

-- | One line for each problem: @file:line:col: message@.
summary :: Failure -> [String]
summary failure = [place p ++ problemMessage p | p <- failureProblems failure]
  where
    place p = case problemSpan p of
      Just (start, _) | not (null (problemFile p)) -> problemFile p ++ ":" ++ show (settle failure p start) ++ ": "
      Just (start, _) -> show (settle failure p start) ++ ": "
      Nothing -> ""

-- | The end of the input is past its last line. Point at the end of that
-- line instead, which is somewhere that can be shown.
settle :: Failure -> Problem -> Pos -> Pos
settle failure p pos
  | posCol pos > 0 = pos
  | otherwise = case lines <$> lookup (problemFile p) (failureSources failure) of
      Just ls@(_ : _) -> Pos (length ls) (length (last ls) + 1)
      _ -> Pos 1 1

diagnostic :: Failure -> Diagnostic String
diagnostic failure = foldl addReport files (map report (failureProblems failure))
  where
    files = foldl (\d (path, src) -> addFile d path src) mempty (failureSources failure)
    report p = Err Nothing (problemTitle p) (marker p) []
    marker p = case problemSpan p of
      Just (start, end) ->
        let Pos l c = settle failure p start
            Pos l' c' = settle failure p end
            stop = if (l', c') > (l, c) then (l', c') else (l, c + 1)
         in [(Position (l, c) stop (problemFile p), This (problemMessage p))]
      Nothing -> []

-- | The problems as text, with the source they are in.
render :: Failure -> String
render failure = unlines (unplaced ++ [show (unAnnotate (prettyDiagnostic WithUnicode (TabSize 4) (diagnostic placed))) | not (null (failureProblems placed))])
  where
    (placed, unplaced) = split failure

-- | Problems with somewhere to point are shown against the source; the
-- others are a line each.
split :: Failure -> (Failure, [String])
split failure =
  ( failure {failureProblems = [p | p <- failureProblems failure, isJust (problemSpan p)]},
    ["error: " ++ problemMessage p | p <- failureProblems failure, isNothing (problemSpan p)]
  )

-- | Write the problems to a handle, in colour if it is a terminal.
printFailure :: Handle -> Failure -> IO ()
printFailure handle failure = do
  terminal <- hIsTerminalDevice handle
  noColour <- lookupEnv "NO_COLOR"
  let (placed, unplaced) = split failure
  hPutStr handle (unlines unplaced)
  if null (failureProblems placed)
    then pure ()
    else
      if terminal && maybe True null noColour
        then printDiagnostic handle WithUnicode (TabSize 4) defaultStyle (diagnostic placed)
        else hPutStr handle (render placed)
