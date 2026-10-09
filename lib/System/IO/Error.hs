-- I/O errors: what kinds there are, what one says of itself, and catching
-- them. The setters and the predicates on kinds are not in the Report's
-- list; they are here because programs written for GHC use them.
module System.IO.Error
  ( IOError, userError, mkIOError, annotateIOError,
    isAlreadyExistsError, isDoesNotExistError, isAlreadyInUseError, isFullError, isEOFError,
    isIllegalOperation, isPermissionError, isUserError,
    ioeGetErrorType, ioeGetErrorString, ioeGetHandle, ioeGetFileName,
    ioeSetErrorType, ioeSetErrorString, ioeSetHandle, ioeSetFileName,
    IOErrorType, alreadyExistsErrorType, doesNotExistErrorType, alreadyInUseErrorType, fullErrorType,
    eofErrorType, illegalOperationErrorType, permissionErrorType, userErrorType,
    isAlreadyExistsErrorType, isDoesNotExistErrorType, isAlreadyInUseErrorType, isFullErrorType,
    isEOFErrorType, isIllegalOperationErrorType, isPermissionErrorType, isUserErrorType,
    ioError, catchIOError, tryIOError, modifyIOError
  )
where

import Control.Monad (mplus)
import Powder.Base (Handle, IOError (..), IOErrorType (..), catchIOError)

tryIOError :: IO a -> IO (Either IOError a)
tryIOError action = catchIOError (action >>= return . Right) (return . Left)

modifyIOError :: (IOError -> IOError) -> IO a -> IO a
modifyIOError f action = catchIOError action (ioError . f)

mkIOError :: IOErrorType -> String -> Maybe Handle -> Maybe FilePath -> IOError
mkIOError t place = IOError t place ""

-- What is given is added; what was there and is not given is kept.
annotateIOError :: IOError -> String -> Maybe Handle -> Maybe FilePath -> IOError
annotateIOError (IOError t _ said h file) place h' file' = IOError t place said (h' `mplus` h) (file' `mplus` file)

alreadyExistsErrorType, doesNotExistErrorType, alreadyInUseErrorType, fullErrorType :: IOErrorType
alreadyExistsErrorType = AlreadyExists
doesNotExistErrorType = NoSuchThing
alreadyInUseErrorType = ResourceBusy
fullErrorType = ResourceExhausted

eofErrorType, illegalOperationErrorType, permissionErrorType, userErrorType :: IOErrorType
eofErrorType = EOF
illegalOperationErrorType = IllegalOperation
permissionErrorType = PermissionDenied
userErrorType = UserError

isAlreadyExistsErrorType, isDoesNotExistErrorType, isAlreadyInUseErrorType, isFullErrorType :: IOErrorType -> Bool
isAlreadyExistsErrorType = (== AlreadyExists)
isDoesNotExistErrorType = (== NoSuchThing)
isAlreadyInUseErrorType = (== ResourceBusy)
isFullErrorType = (== ResourceExhausted)

isEOFErrorType, isIllegalOperationErrorType, isPermissionErrorType, isUserErrorType :: IOErrorType -> Bool
isEOFErrorType = (== EOF)
isIllegalOperationErrorType = (== IllegalOperation)
isPermissionErrorType = (== PermissionDenied)
isUserErrorType = (== UserError)

isAlreadyExistsError, isDoesNotExistError, isAlreadyInUseError, isFullError :: IOError -> Bool
isAlreadyExistsError = isAlreadyExistsErrorType . ioeGetErrorType
isDoesNotExistError = isDoesNotExistErrorType . ioeGetErrorType
isAlreadyInUseError = isAlreadyInUseErrorType . ioeGetErrorType
isFullError = isFullErrorType . ioeGetErrorType

isEOFError, isIllegalOperation, isPermissionError, isUserError :: IOError -> Bool
isEOFError = isEOFErrorType . ioeGetErrorType
isIllegalOperation = isIllegalOperationErrorType . ioeGetErrorType
isPermissionError = isPermissionErrorType . ioeGetErrorType
isUserError = isUserErrorType . ioeGetErrorType

ioeGetErrorType :: IOError -> IOErrorType
ioeGetErrorType (IOError t _ _ _ _) = t

-- What a program's own error was given to say; of any other, its kind.
ioeGetErrorString :: IOError -> String
ioeGetErrorString (IOError t _ said _ _) = if t == UserError then said else show t

ioeGetHandle :: IOError -> Maybe Handle
ioeGetHandle (IOError _ _ _ h _) = h

ioeGetFileName :: IOError -> Maybe FilePath
ioeGetFileName (IOError _ _ _ _ file) = file

ioeSetErrorType :: IOError -> IOErrorType -> IOError
ioeSetErrorType (IOError _ place said h file) t = IOError t place said h file

ioeSetErrorString :: IOError -> String -> IOError
ioeSetErrorString (IOError t place _ h file) said = IOError t place said h file

ioeSetHandle :: IOError -> Handle -> IOError
ioeSetHandle (IOError t place said _ file) h = IOError t place said (Just h) file

ioeSetFileName :: IOError -> FilePath -> IOError
ioeSetFileName (IOError t place said h _) file = IOError t place said h (Just file)
