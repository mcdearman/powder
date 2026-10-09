-- Haskell 2010 Report 42.1.1, 41 (System.IO.Error): errors raised by real I/O operations: a nonexistent
-- file (isDoesNotExistError, ioeGetFileName), reading at end of file (isEOFError), operations on closed
-- and semi-closed handles and in the wrong mode (isIllegalOperation), fail (isUserError)
module Main (main) where

import System.IO
import System.IO.Error

path :: FilePath
path = "/tmp/powder-conformance-real-errors-doesNotExist-eof-illegalOperation"

missing :: FilePath
missing = "/tmp/powder-conformance-no-such-directory-x7q/missing.txt"

classify :: IOError -> String
classify e =
  unwords
    [ name
    | (name, p) <-
        [ ("alreadyExists", isAlreadyExistsError)
        , ("doesNotExist", isDoesNotExistError)
        , ("alreadyInUse", isAlreadyInUseError)
        , ("full", isFullError)
        , ("eof", isEOFError)
        , ("illegalOperation", isIllegalOperation)
        , ("permission", isPermissionError)
        , ("user", isUserError)
        ]
    , p e
    ]

attempt :: String -> IO a -> IO ()
attempt name act = do
  r <- tryIOError act
  putStrLn (name ++ ": " ++ either classify (const "no error") r)

main :: IO ()
main = do
  attempt "readFile of a missing file" (readFile missing)
  attempt "openFile ReadMode of a missing file" (openFile missing ReadMode)
  attempt "writeFile into a missing directory" (writeFile missing "x")
  attempt "appendFile into a missing directory" (appendFile missing "x")
  attempt "withFile of a missing file" (withFile missing ReadMode hGetLine)
  r <- tryIOError (readFile missing)
  print ("file name of the failed open", either ioeGetFileName (const Nothing) r == Just missing)
  writeFile path "one\n"
  h <- openFile path ReadMode
  l <- hGetLine h
  print ("the only line", l)
  attempt "hGetLine at end of file" (hGetLine h)
  attempt "hGetChar at end of file" (hGetChar h)
  attempt "hLookAhead at end of file" (hLookAhead h)
  attempt "hIsEOF at end of file" (hIsEOF h)
  attempt "hPutStr on a read handle" (hPutStr h "x")
  attempt "hPutChar on a read handle" (hPutChar h 'x')
  hSeek h AbsoluteSeek 0
  again <- hGetLine h
  print ("the handle is still usable after errors", again)
  hClose h
  attempt "hGetChar on a closed handle" (hGetChar h)
  attempt "hGetLine on a closed handle" (hGetLine h)
  attempt "hGetContents on a closed handle" (hGetContents h)
  attempt "hIsEOF on a closed handle" (hIsEOF h)
  attempt "hSeek on a closed handle" (hSeek h AbsoluteSeek 0)
  attempt "hFileSize on a closed handle" (hFileSize h)
  attempt "hClose on a closed handle" (hClose h)
  w <- openFile path WriteMode
  attempt "hGetLine on a write handle" (hGetLine w)
  attempt "hGetChar on a write handle" (hGetChar w)
  hPutStrLn w "still writable"
  hClose w
  attempt "hPutStr on a closed handle" (hPutStr w "x")
  attempt "hFlush on a closed handle" (hFlush w)
  s <- openFile path ReadMode
  contents <- hGetContents s
  attempt "hGetChar on a semi-closed handle" (hGetChar s)
  attempt "hGetContents on a semi-closed handle" (hGetContents s)
  print ("contents are still delivered", contents)
  hClose s
  attempt "ioError (userError ...)" (ioError (userError "u"))
  attempt "fail in IO" (fail "f" :: IO ())
  attempt "readIO parse failure" (readIO "zzz" :: IO Int)
  attempt "no error" (return ())
