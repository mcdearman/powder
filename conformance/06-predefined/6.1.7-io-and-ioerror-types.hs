-- Haskell 2010 Report 6.1.7: the IO type (abstract, instance of Functor and Monad) and the IOError type
import System.IO.Error (catchIOError, ioeGetErrorString, isUserError)

answer :: IO Int
answer = return 41

failing :: IO Int
failing = ioError (userError "bad")

handler :: IOError -> IO Int
handler e = return (if isUserError e then length (ioeGetErrorString e) else (-1))

describe :: IOError -> String
describe e
  | isUserError e = "user error: " ++ ioeGetErrorString e
  | otherwise = "other error"

greet :: String -> IO ()
greet name = putStrLn ("hello " ++ name)

main :: IO ()
main = do
  x <- answer
  print ("return then bind", x)
  y <- fmap (+ 1) answer
  print ("fmap on IO", y)
  z <- answer >>= \v -> return (v * 2)
  print ("bind", z)
  let actions = [greet "a", greet "b"] :: [IO ()]
  print ("IO values in a list are not run by length", length actions)
  sequence_ actions
  r1 <- catchIOError answer handler
  print ("no error", r1)
  r2 <- catchIOError failing handler
  print ("user error caught", r2)
  let e1 = userError "one" :: IOError
  print ("userError builds a value without raising", describe e1)
  print ("IOError is in Eq", e1 == userError "one", e1 == userError "two", e1 /= userError "two")
  r3 <- catchIOError (ioError e1 >> return "not reached") (\e -> return (describe e))
  putStrLn r3
  unit <- greet "c"
  print ("IO () yields unit", unit)
