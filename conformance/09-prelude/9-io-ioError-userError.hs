-- Haskell 2010 Report 9 (PreludeIO): IOError, ioError, userError (caught with System.IO.Error.catchIOError;
-- the Report's Prelude.catch does not exist in GHC and is not used)
module Main (main) where

import System.IO.Error (catchIOError, ioeGetErrorString, isDoesNotExistError, isUserError)

boom :: IOError
boom = userError "boom"

safeDiv :: Int -> Int -> IO Int
safeDiv _ 0 = ioError (userError "division by zero")
safeDiv a b = return (a `div` b)

describe :: IOError -> IO String
describe e = return ((if isUserError e then "user error: " else "other error: ") ++ ioeGetErrorString e)

main :: IO ()
main = do
  r1 <- catchIOError (ioError boom >> return "not reached") (\_ -> return "caught")
  print ("ioError raises", r1)
  r2 <- catchIOError (ioError boom) describe
  print ("handler receives the error", r2)
  r3 <- catchIOError (fmap show (safeDiv 10 2)) describe
  r4 <- catchIOError (fmap show (safeDiv 10 0)) describe
  print ("safeDiv", r3, r4)
  r5 <- catchIOError (return "no error") (\_ -> return "handler not run")
  print ("no error", r5)
  r6 <- catchIOError (putStrLn "before" >> ioError boom >> putStrLn "after" >> return 'n') (\_ -> return 'y')
  print ("rest of the action skipped", r6)
  r7 <- catchIOError (catchIOError (ioError boom) (\_ -> ioError (userError "second"))) describe
  print ("error raised in handler", r7)
  r8 <- catchIOError (catchIOError (ioError boom) (\e -> ioError e)) describe
  print ("rethrown", r8)
  r9 <- catchIOError (fail "from fail") describe
  print ("fail in IO is a user error", r9)
  r10 <- catchIOError (readFile "/tmp/powder-conformance-9-io-ioError-userError/does/not/exist") (\e -> return (if isDoesNotExistError e then "does not exist" else "some other error"))
  print ("readFile of a missing file", r10)
  print ("userError is pure", isUserError boom, isUserError (userError ""), ioeGetErrorString (userError "msg"))
  print ("IOError Eq", boom == userError "boom", boom == userError "other", boom /= userError "other")
  let errors = [userError "a", userError "b"] :: [IOError]
  print ("IOError values", map ioeGetErrorString errors, length errors)
  r11 <- catchIOError (mapM (safeDiv 100) [1, 2, 0, 5]) (\_ -> return [])
  print ("mapM stops at first error", r11)
