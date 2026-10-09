-- Haskell 2010 Report 42.1, 42.3 (System.IO.Error): IOError, userError, ioError, and the Report's catch
-- and try (named catchIOError and tryIOError in current libraries)
module Main (main) where

import System.IO.Error

boom :: String -> IO a
boom msg = ioError (userError msg)

safeDivide :: Int -> Int -> IO Int
safeDivide _ 0 = boom "division by zero"
safeDivide x y = return (x `div` y)

main :: IO ()
main = do
  r1 <- catchIOError (boom "first" >> return "not reached") (\e -> return ("caught: " ++ ioeGetErrorString e))
  putStrLn r1
  r2 <- catchIOError (return "no error") (\_ -> return "handler ran")
  putStrLn r2
  catchIOError (putStrLn "effects before the error happen" >> boom "second" >> putStrLn "not reached") (\e -> putStrLn ("handler: " ++ ioeGetErrorString e))
  r3 <- catchIOError (safeDivide 10 2) (\_ -> return (-1))
  r4 <- catchIOError (safeDivide 10 0) (\_ -> return (-1))
  print ("safeDivide", r3, r4)
  r5 <- catchIOError (catchIOError (boom "inner") (\e -> return ("inner handler: " ++ ioeGetErrorString e))) (\_ -> return "outer handler")
  putStrLn r5
  r6 <- catchIOError (catchIOError (boom "rethrown") (\e -> ioError e)) (\e -> return ("outer handler: " ++ ioeGetErrorString e))
  putStrLn r6
  r7 <- catchIOError (catchIOError (boom "original") (\_ -> boom "replaced in handler")) (\e -> return ("outer handler: " ++ ioeGetErrorString e))
  putStrLn r7
  r8 <- catchIOError (boom "a" `catchIOError` (\_ -> return "left to right")) (\_ -> return "outer")
  putStrLn r8
  t1 <- tryIOError (boom "tried")
  putStrLn (either (\e -> "Left: " ++ ioeGetErrorString e) (\() -> "Right") t1)
  t2 <- tryIOError (return (42 :: Int))
  putStrLn (either (\e -> "Left: " ++ ioeGetErrorString e) (\n -> "Right: " ++ show n) t2)
  t3 <- tryIOError (safeDivide 7 0)
  print ("tryIOError safeDivide", either (const Nothing) Just t3)
  t4 <- tryIOError (mapM (safeDivide 100) [1, 2, 0, 5])
  print ("error aborts mapM", either ioeGetErrorString show t4)
  t5 <- tryIOError (tryIOError (boom "nested try"))
  putStrLn (either (const "outer Left") (either (\e -> "inner Left: " ++ ioeGetErrorString e) (\() -> "inner Right")) t5)
  t6 <- tryIOError (fail "fail in IO is a user error")
  putStrLn (either (\e -> ioeGetErrorString e ++ " / " ++ show (isUserError e)) (\() -> "Right") t6)
  let e1 = userError "a value, not an action"
  print ("userError builds a value", isUserError e1, ioeGetErrorString e1)
  print ("userError with empty string", ioeGetErrorString (userError ""), null (ioeGetErrorString (userError "")))
  t7 <- tryIOError (ioError e1 >> return ())
  print ("thrown later", either ioeGetErrorString (const "no error") t7)
  results <- mapM (\n -> tryIOError (safeDivide 12 n)) [3, 0, 4]
  print ("recover per element", map (either (const 0) id) results)
  putStrLn "done"
