-- Haskell 2010 Report 42.1, 42.1.2 (System.IO.Error): attributes of I/O errors: ioeGetErrorString (printed
-- for user errors only), ioeGetHandle (only its presence is printed), ioeGetFileName; annotateIOError
module Main (main) where

import Data.Maybe (isJust, isNothing)
import System.IO
import System.IO.Error

plain :: IOError
plain = mkIOError doesNotExistErrorType "opening" Nothing Nothing

withName :: IOError
withName = mkIOError doesNotExistErrorType "opening" Nothing (Just "/tmp/some-name")

withHandle :: IOError
withHandle = mkIOError eofErrorType "reading" (Just stdin) Nothing

withBoth :: IOError
withBoth = mkIOError illegalOperationErrorType "writing" (Just stdout) (Just "/tmp/other-name")

main :: IO ()
main = do
  print ("user error string", ioeGetErrorString (userError "the message"))
  print ("user error string with special characters", ioeGetErrorString (userError "line1\nline2 \"quoted\""))
  print ("user error has no file name", ioeGetFileName (userError "u"))
  print ("user error has no handle", isNothing (ioeGetHandle (userError "u")))
  print ("plain: file name", ioeGetFileName plain)
  print ("plain: handle absent", isNothing (ioeGetHandle plain))
  print ("with name: file name", ioeGetFileName withName)
  print ("with name: handle absent", isNothing (ioeGetHandle withName))
  print ("with handle: file name", ioeGetFileName withHandle)
  print ("with handle: handle present", isJust (ioeGetHandle withHandle))
  print ("with both: file name", ioeGetFileName withBoth)
  print ("with both: handle present", isJust (ioeGetHandle withBoth))
  print ("error string of a non-user error is a string", length (ioeGetErrorString plain) >= 0)
  let a1 = annotateIOError plain "annotated" Nothing (Just "/tmp/added-name")
  print ("annotate adds a file name", ioeGetFileName a1, isNothing (ioeGetHandle a1))
  let a2 = annotateIOError plain "annotated" (Just stderr) Nothing
  print ("annotate adds a handle", ioeGetFileName a2, isJust (ioeGetHandle a2))
  let a3 = annotateIOError plain "annotated" (Just stderr) (Just "/tmp/added-name")
  print ("annotate adds both", ioeGetFileName a3, isJust (ioeGetHandle a3))
  let a4 = annotateIOError withBoth "annotated" Nothing Nothing
  print ("annotate with nothing keeps both", ioeGetFileName a4, isJust (ioeGetHandle a4))
  let a5 = annotateIOError withName "annotated" (Just stdin) Nothing
  print ("annotate keeps an existing file name", ioeGetFileName a5, isJust (ioeGetHandle a5))
  let a6 = annotateIOError withHandle "annotated" Nothing (Just "/tmp/added-name")
  print ("annotate keeps an existing handle", ioeGetFileName a6, isJust (ioeGetHandle a6))
  print ("annotate keeps the error type", isDoesNotExistError a1, isDoesNotExistError a3, isIllegalOperation a4, isEOFError a6)
  let u = annotateIOError (userError "kept message") "annotated" Nothing (Just "/tmp/user-file")
  print ("annotated user error", isUserError u, ioeGetFileName u)
  r <- tryIOError (ioError a3)
  print ("attributes survive throwing", either ioeGetFileName (const Nothing) r, either (isJust . ioeGetHandle) (const False) r)
  r2 <- catchIOError (ioError withName >> return "not reached") (\e -> return (maybe "no file" (\f -> "file: " ++ f) (ioeGetFileName e)))
  putStrLn r2
