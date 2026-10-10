-- Haskell 2010 Report 25 (Foreign.C.Error): errno after a failed C call, resetErrno, errnoToIOError, and
-- the throwErrno family. Errors are classified; their text, which is the system's, is not printed.
module Main (main) where

import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import System.IO.Error

foreign import ccall "unistd.h close" c_close :: CInt -> IO CInt
foreign import ccall "stdio.h fopen" c_fopen :: CString -> CString -> IO (Ptr ())
foreign import ccall "stdlib.h abs" c_abs :: CInt -> IO CInt

kind :: IOError -> String
kind e
  | isDoesNotExistError e = "does not exist"
  | isAlreadyExistsError e = "already exists"
  | isPermissionError e = "permission"
  | isAlreadyInUseError e = "in use"
  | isFullError e = "full"
  | isIllegalOperation e = "illegal operation"
  | otherwise = "other"

attempt :: Show a => String -> IO a -> IO ()
attempt label act = tryIOError act >>= \r -> putStrLn (label ++ ": " ++ either (\e -> "raised, " ++ kind e) show r)

missing :: (CString -> CString -> IO a) -> IO a
missing k = withCString "/powder-conformance-no-such-directory/file" (\path -> withCString "r" (k path))

main :: IO ()
main = do
  r <- c_close (-1)
  e <- getErrno
  print ("close of a bad descriptor", r, e == eBADF, e == eOK, e /= eNOENT)
  p <- missing c_fopen
  e2 <- getErrno
  print ("fopen of a missing file", p == nullPtr, e2 == eNOENT)
  resetErrno
  e3 <- getErrno
  print ("resetErrno", e3 == eOK)
  print ("isValidErrno", isValidErrno eOK, isValidErrno eNOENT, isValidErrno (Errno (-1)))
  print ("errors are told apart", eNOENT == eNOENT, eNOENT == eEXIST, eACCES /= ePERM, eAGAIN == eWOULDBLOCK)
  print ("errnoToIOError", kind (errnoToIOError "here" eNOENT Nothing Nothing), kind (errnoToIOError "here" eEXIST Nothing Nothing), kind (errnoToIOError "here" eACCES Nothing Nothing), kind (errnoToIOError "here" eNOSPC Nothing Nothing), kind (errnoToIOError "here" eBUSY Nothing Nothing))
  print ("the file name is kept", ioeGetFileName (errnoToIOError "here" eNOENT Nothing (Just "/some/file")))
  attempt "throwErrnoIfMinus1 passes" (throwErrnoIfMinus1 "abs" (c_abs (-4)))
  attempt "throwErrnoIfMinus1 raises" (throwErrnoIfMinus1 "close" (c_close (-1)))
  attempt "throwErrnoIfMinus1_ raises" (throwErrnoIfMinus1_ "close" (c_close (-1)))
  attempt "throwErrnoIfNull raises" (throwErrnoIfNull "fopen" (missing c_fopen) >>= \q -> return (q == nullPtr))
  attempt "throwErrnoIf with its own test" (throwErrnoIf (< 0) "close" (c_close (-1)))
  attempt "throwErrnoIf_ passes" (throwErrnoIf_ (< 0) "abs" (c_abs 3))
  attempt "throwErrnoIfMinus1Retry raises what is not an interruption" (throwErrnoIfMinus1Retry "close" (c_close (-1)))
  attempt "throwErrnoIfNullRetry raises" (throwErrnoIfNullRetry "fopen" (missing c_fopen) >>= \q -> return (q == nullPtr))
  attempt "throwErrno" (missing c_fopen >> throwErrno "after fopen" :: IO ())
