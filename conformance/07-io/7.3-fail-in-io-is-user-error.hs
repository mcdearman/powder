-- Haskell 2010 Report 7.3: fail in the IO monad raises a user error (fail s = ioError (userError s));
-- only the classification is printed, since the message text attached by fail may differ between implementations
import System.IO.Error (catchIOError, ioeGetErrorString, isUserError)

handler :: String -> IOError -> IO ()
handler label e = putStrLn (label ++ ": isUserError=" ++ show (isUserError e))

firstTwo :: [Int] -> IO (Int, Int)
firstTwo xs = do
  (a : b : _) <- return xs
  return (a, b)

main :: IO ()
main = do
  catchIOError (fail "explicit fail") (handler "explicit fail")
  catchIOError (fail "fail then more" >> putStrLn "must not print") (handler "fail stops the sequence")
  r <- catchIOError (fail "typed" :: IO Int) (\e -> return (if isUserError e then 1 else 0))
  print ("fail at IO Int", r)
  p <- catchIOError (firstTwo [1, 2, 3]) (\_ -> return (0, 0))
  print ("pattern matched", p)
  q <- catchIOError (firstTwo [1]) (\e -> return (if isUserError e then (-1, -1) else (-2, -2)))
  print ("pattern failed, user error caught", q)
  catchIOError (do { Just x <- return (Nothing :: Maybe Int); print x }) (handler "Nothing against Just")
  catchIOError (do { Just x <- return (Just (5 :: Int)); print ("matched", x) }) (handler "must not run")
  catchIOError (do { 'a' <- return 'b'; putStrLn "must not print" }) (handler "literal pattern")
  catchIOError (ioError (userError "plain userError")) (\e -> putStrLn ("ioError (userError s) keeps s: " ++ ioeGetErrorString e))
  n <- catchIOError (mapM (\x -> if x > 2 then fail "too big" else return (x * 2)) [1, 2, 3, 4 :: Int]) (\_ -> return [])
  print ("mapM aborted by fail", n)
  putStrLn "done"
