-- Haskell 2010 Report 7.3 / 7.1: with empty standard input, getChar, getLine and readLn raise an EOF error; getContents returns ""
import System.IO.Error (catchIOError, isEOFError, isUserError)

kind :: IOError -> String
kind e
  | isEOFError e = "EOF error"
  | isUserError e = "user error"
  | otherwise = "other error"

main :: IO ()
main = do
  a <- catchIOError (fmap show getChar) (return . kind)
  putStrLn ("getChar: " ++ a)
  b <- catchIOError (fmap show getLine) (return . kind)
  putStrLn ("getLine: " ++ b)
  c <- catchIOError (fmap show (readLn :: IO Int)) (return . kind)
  putStrLn ("readLn: " ++ c)
  d <- catchIOError (getChar >> getChar >> return "two chars") (return . kind)
  putStrLn ("getChar twice: " ++ d)
  s <- getContents
  print ("getContents on empty input", s, null s)
  putStrLn "done"
