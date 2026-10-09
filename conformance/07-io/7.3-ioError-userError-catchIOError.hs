-- Haskell 2010 Report 7.3: ioError raises an IOError, userError builds one; caught here with catchIOError
-- (the Report's Prelude.catch; modern GHC only provides it as System.IO.Error.catchIOError)
import System.IO.Error (catchIOError, ioeGetErrorString, isDoesNotExistError, isEOFError, isUserError)

report :: IOError -> IO ()
report e = putStrLn ("caught: isUserError=" ++ show (isUserError e) ++ " message=" ++ show (ioeGetErrorString e))

safeDivide :: Int -> Int -> IO Int
safeDivide _ 0 = ioError (userError "division by zero")
safeDivide a b = return (a `div` b)

main :: IO ()
main = do
  catchIOError (ioError (userError "boom")) report
  catchIOError (putStrLn "no error: handler not used") report
  r1 <- catchIOError (safeDivide 10 2) (\_ -> return (-1))
  print ("result without error", r1)
  r2 <- catchIOError (safeDivide 10 0) (\_ -> return (-1))
  print ("result from handler", r2)
  r3 <- catchIOError (safeDivide 1 0 >>= \x -> return (show x)) (\e -> return (ioeGetErrorString e))
  print ("message from handler", r3)
  catchIOError (putStrLn "before the error" >> ioError (userError "mid") >> putStrLn "must not print") report
  catchIOError (ioError (userError "skips binds") >>= \x -> putStrLn ("must not print" ++ x)) report
  catchIOError (do { putStrLn "in do"; _ <- safeDivide 1 0; putStrLn "must not print" }) report
  catchIOError (mapM_ (\n -> safeDivide 12 n >>= print) [3, 2, 0, 1]) report
  let e = userError "a value"
  print ("userError alone raises nothing", isUserError e, ioeGetErrorString e)
  print ("other predicates on a user error", isDoesNotExistError e, isEOFError e)
  catchIOError (ioError e) report
  catchIOError (ioError e) report
  catchIOError (ioError (userError "")) report
  catchIOError (ioError (userError "multi word message with \"quotes\"")) report
  r4 <- catchIOError (fmap Right (safeDivide 7 0)) (return . Left . ioeGetErrorString)
  print ("as Either", r4 :: Either String Int)
  r5 <- catchIOError (fmap Right (safeDivide 7 7)) (return . Left . ioeGetErrorString)
  print ("as Either", r5 :: Either String Int)
  putStrLn "execution continues after handled errors"
