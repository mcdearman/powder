-- Haskell 2010 Report 7.1 / 7.3: readIO (and readLn) signal a parse failure as a user error in the IO monad instead of calling error
import System.IO.Error (catchIOError, isEOFError, isUserError)

tryRead :: String -> IO String
tryRead s = catchIOError (fmap describe (readIO s)) (\e -> return ("failed, isUserError=" ++ show (isUserError e)))
  where
    describe :: Int -> String
    describe n = "ok " ++ show n

main :: IO ()
main = do
  mapM_ (\s -> tryRead s >>= \r -> putStrLn (show s ++ " -> " ++ r)) ["12", " 12 ", "(12)", "-12", "twelve", "", "12 13", "12x", "1.5", "[12]"]
  b <- catchIOError (readIO "maybe" :: IO Bool) (\_ -> return False)
  print ("handler supplies a default", b)
  xs <- catchIOError (mapM readIO ["1", "2", "x", "4"] :: IO [Int]) (\_ -> return [])
  print ("mapM readIO aborted", xs)
  l1 <- catchIOError (fmap Right (readLn :: IO Int)) (return . Left)
  putStrLn ("readLn good line: " ++ either (const "error") show l1)
  l2 <- catchIOError (fmap Right (readLn :: IO Int)) (return . Left)
  putStrLn ("readLn bad line: " ++ either (\e -> "isUserError=" ++ show (isUserError e) ++ " isEOFError=" ++ show (isEOFError e)) show l2)
  l3 <- catchIOError (fmap Right (readLn :: IO Int)) (return . Left)
  putStrLn ("readLn continues with the next line: " ++ either (const "error") show l3)
