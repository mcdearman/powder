-- Haskell 2010 Report 7.3: reading a file that does not exist raises an IOError satisfying isDoesNotExistError
import System.IO.Error (catchIOError, isAlreadyExistsError, isDoesNotExistError, isEOFError, isUserError)

missing :: FilePath
missing = "/tmp/powder-conformance-7.3-isDoesNotExistError-missing-file/definitely-missing"

classify :: IOError -> String
classify e
  | isDoesNotExistError e = "does not exist"
  | isUserError e = "user error"
  | isEOFError e = "end of file"
  | isAlreadyExistsError e = "already exists"
  | otherwise = "something else"

attempt :: IO a -> IO (Either IOError a)
attempt act = catchIOError (fmap Right act) (return . Left)

main :: IO ()
main = do
  r <- attempt (readFile missing)
  case r of
    Left e -> do
      putStrLn ("readFile: " ++ classify e)
      print ("isDoesNotExistError", isDoesNotExistError e)
      print ("other predicates", isUserError e, isEOFError e, isAlreadyExistsError e)
    Right s -> putStrLn ("unexpectedly read " ++ show (length s) ++ " characters")
  n <- catchIOError (fmap length (readFile missing)) (\e -> return (if isDoesNotExistError e then (-1) else (-2)))
  print ("length or error code", n)
  w <- attempt (writeFile missing "x")
  putStrLn ("writeFile into a missing directory: " ++ either classify (const "succeeded") w)
  a <- attempt (appendFile missing "x")
  putStrLn ("appendFile into a missing directory: " ++ either classify (const "succeeded") a)
  catchIOError
    (readFile missing >>= \s -> putStrLn ("must not print" ++ s))
    (\e -> if isDoesNotExistError e then putStrLn "handler selected by predicate" else ioError e)
  u <- attempt (ioError (userError "not a file error"))
  putStrLn ("user error for contrast: " ++ either classify (const "succeeded") u)
  putStrLn "program continues"
