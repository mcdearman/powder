-- Haskell 2010 Report 7.3: nested handlers; the innermost handler runs first, a handler may re-raise or raise anew
import System.IO.Error (catchIOError, ioeGetErrorString, isUserError)

raise :: String -> IO a
raise = ioError . userError

note :: String -> IOError -> IO ()
note who e = putStrLn (who ++ " handler got: " ++ ioeGetErrorString e)

main :: IO ()
main = do
  -- inner handles, outer never sees it
  catchIOError
    (catchIOError (raise "e1") (note "inner") >> putStrLn "inner recovered, continuing")
    (note "outer (must not run)")
  -- inner re-raises the same error
  catchIOError
    (catchIOError (raise "e2") (\e -> note "inner" e >> ioError e))
    (note "outer")
  -- inner raises a different error
  catchIOError
    (catchIOError (raise "e3") (\e -> note "inner" e >> raise ("wrapped " ++ ioeGetErrorString e)))
    (note "outer")
  -- an error raised in a handler is not caught by that same handler's catch
  r <-
    catchIOError
      (catchIOError (raise "e4") (\_ -> raise "from handler") >> return "not reached")
      (\e -> return ("outer saw: " ++ ioeGetErrorString e))
  putStrLn r
  -- an error after the inner catch has completed goes to the outer handler only
  catchIOError
    (catchIOError (putStrLn "inner ok") (note "inner (must not run)") >> raise "e5")
    (note "outer")
  -- three levels, re-raised twice
  catchIOError
    ( catchIOError
        (catchIOError (raise "e6") (\e -> note "level 3" e >> ioError e))
        (\e -> note "level 2" e >> ioError e)
    )
    (note "level 1")
  -- a handler that itself uses catch
  catchIOError
    (raise "e7")
    (\e -> catchIOError (note "outer" e >> raise "inside handler") (note "handler's own"))
  -- the handler's result is the result of the whole catch
  n <- catchIOError (catchIOError (raise "e8") (\_ -> return (1 :: Int))) (\_ -> return 2)
  m <- catchIOError (catchIOError (raise "e9") (\e -> ioError e)) (\_ -> return (2 :: Int))
  print ("results", n, m)
  -- selective handler: re-raise unless the message matches
  let selective msg act = catchIOError act (\e -> if ioeGetErrorString e == msg then putStrLn ("selected " ++ msg) else ioError e)
  catchIOError (selective "x" (selective "y" (raise "x"))) (note "outermost (must not run)")
  catchIOError (selective "x" (selective "y" (raise "z"))) (note "outermost")
  print ("user errors throughout", True && isUserError (userError "u"))
