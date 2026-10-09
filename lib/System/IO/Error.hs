module System.IO.Error
  ( IOError, userError, ioError, catchIOError, tryIOError, ioeGetErrorString,
    isUserError, isDoesNotExistError, isEOFError, isAlreadyExistsError, isAlreadyInUseError, isFullError,
    isIllegalOperation, isPermissionError
  )
where

import Powder.Base (IOError (..), catchIOError)

tryIOError :: IO a -> IO (Either IOError a)
tryIOError action = catchIOError (action >>= return . Right) (return . Left)

kind :: IOError -> Int
kind (IOError k _) = k

isUserError, isDoesNotExistError, isEOFError, isAlreadyExistsError :: IOError -> Bool
isUserError e = kind e == 0
isDoesNotExistError e = kind e == 1
isEOFError e = kind e == 2
isAlreadyExistsError e = kind e == 3

-- Nothing here raises these.
isAlreadyInUseError, isFullError, isIllegalOperation, isPermissionError :: IOError -> Bool
isAlreadyInUseError _ = False
isFullError _ = False
isIllegalOperation _ = False
isPermissionError _ = False

ioeGetErrorString :: IOError -> String
ioeGetErrorString (IOError _ s) = s
