-- Haskell 2010 Report 42 (System.IO.Error): names GHC's System.IO.Error exports beyond the Report's
-- export list: ioeGetErrorType, isAlreadyExistsErrorType ... isUserErrorType, modifyIOError,
-- ioeSetErrorType, ioeSetErrorString, ioeSetHandle, ioeSetFileName. A strictly conforming Haskell 2010
-- library does not export them (the imports would be rejected), hence -ghc-differs.
module Main (main) where

import Data.Maybe (isJust)
import System.IO
import System.IO.Error

types :: [(String, IOErrorType)]
types =
  [ ("alreadyExists", alreadyExistsErrorType)
  , ("doesNotExist", doesNotExistErrorType)
  , ("alreadyInUse", alreadyInUseErrorType)
  , ("full", fullErrorType)
  , ("eof", eofErrorType)
  , ("illegalOperation", illegalOperationErrorType)
  , ("permission", permissionErrorType)
  , ("user", userErrorType)
  ]

typePredicates :: [IOErrorType -> Bool]
typePredicates =
  [ isAlreadyExistsErrorType
  , isDoesNotExistErrorType
  , isAlreadyInUseErrorType
  , isFullErrorType
  , isEOFErrorType
  , isIllegalOperationErrorType
  , isPermissionErrorType
  , isUserErrorType
  ]

main :: IO ()
main = do
  mapM_ (\(name, t) -> putStrLn (name ++ ": " ++ show (map (\p -> p t) typePredicates))) types
  print ("ioeGetErrorType of mkIOError", all (\(_, t) -> ioeGetErrorType (mkIOError t "loc" Nothing Nothing) == t) types)
  print ("ioeGetErrorType of userError", isUserErrorType (ioeGetErrorType (userError "u")))
  let e = mkIOError doesNotExistErrorType "loc" Nothing (Just "/tmp/a")
  print ("ioeSetErrorType", isPermissionError (ioeSetErrorType e permissionErrorType), isDoesNotExistError (ioeSetErrorType e permissionErrorType))
  print ("ioeSetErrorType keeps the file name", ioeGetFileName (ioeSetErrorType e permissionErrorType))
  print ("ioeSetFileName", ioeGetFileName (ioeSetFileName e "/tmp/b"))
  print ("ioeSetHandle", isJust (ioeGetHandle e), isJust (ioeGetHandle (ioeSetHandle e stdout)))
  print ("ioeSetErrorString on a user error", ioeGetErrorString (ioeSetErrorString (userError "old") "new"))
  r <- tryIOError (modifyIOError (\err -> ioeSetFileName err "/tmp/modified") (ioError e))
  print ("modifyIOError rewrites the error", either ioeGetFileName (const Nothing) r)
  r2 <- tryIOError (modifyIOError (\_ -> userError "replaced") (ioError e))
  print ("modifyIOError can replace the error", either ioeGetErrorString (const "no error") r2)
  r3 <- modifyIOError (\_ -> userError "unused") (return "no error, so the function is not applied")
  putStrLn r3
  r4 <- tryIOError (modifyIOError (\err -> ioeSetErrorType err eofErrorType) (ioError (userError "u")))
  print ("modifyIOError changing the type", either isEOFError (const False) r4)
