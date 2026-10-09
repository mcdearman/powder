-- Haskell 2010 Report 42.1.1, 42.2 (System.IO.Error): mkIOError with every IOErrorType
-- (alreadyExistsErrorType ... userErrorType) classified by every predicate (isAlreadyExistsError ...
-- isUserError); Eq on IOErrorType
module Main (main) where

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

predicates :: [(String, IOError -> Bool)]
predicates =
  [ ("isAlreadyExistsError", isAlreadyExistsError)
  , ("isDoesNotExistError", isDoesNotExistError)
  , ("isAlreadyInUseError", isAlreadyInUseError)
  , ("isFullError", isFullError)
  , ("isEOFError", isEOFError)
  , ("isIllegalOperation", isIllegalOperation)
  , ("isPermissionError", isPermissionError)
  , ("isUserError", isUserError)
  ]

make :: IOErrorType -> IOError
make t = mkIOError t "somewhere" Nothing Nothing

classify :: IOError -> [String]
classify e = [name | (name, p) <- predicates, p e]

main :: IO ()
main = do
  mapM_ (\(name, t) -> putStrLn (name ++ ": " ++ show (map (\(_, p) -> p (make t)) predicates))) types
  mapM_ (\(name, t) -> putStrLn (name ++ " satisfies " ++ show (classify (make t)))) types
  print ("each type satisfies exactly one predicate", all (\(_, t) -> length (classify (make t)) == 1) types)
  print ("type equality is reflexive", all (\(_, t) -> t == t) types)
  print ("types are pairwise distinct", and [(t1 == t2) == (n1 == n2) | (n1, t1) <- types, (n2, t2) <- types])
  print ("inequality", alreadyExistsErrorType /= doesNotExistErrorType, eofErrorType /= eofErrorType)
  print ("userError is a user error", classify (userError "u"))
  r <- tryIOError (ioError (make eofErrorType))
  print ("a made error can be thrown and caught", either classify (const []) r)
  r2 <- catchIOError (ioError (make permissionErrorType) >> return "not reached") (\e -> return (if isPermissionError e then "permission" else "other"))
  print ("handler dispatches on the predicate", r2)
  r3 <- catchIOError (catchIOError (ioError (make fullErrorType)) (\e -> if isEOFError e then return "wrong handler" else ioError e)) (\e -> return (if isFullError e then "rethrown to the outer handler" else "other"))
  print ("selective handler", r3)
  print ("with file name", classify (mkIOError doesNotExistErrorType "open" Nothing (Just "/no/such/file")))
  print ("location text does not affect classification", classify (mkIOError alreadyInUseErrorType "" Nothing Nothing))
