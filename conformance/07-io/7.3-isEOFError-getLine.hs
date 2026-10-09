-- Haskell 2010 Report 7.3 / 7.1: getLine at end of input raises an IOError satisfying isEOFError
import System.IO.Error (catchIOError, isDoesNotExistError, isEOFError, isUserError)

readAllLines :: IO [String]
readAllLines = catchIOError more (\e -> if isEOFError e then return [] else ioError e)
  where
    more = do
      l <- getLine
      ls <- readAllLines
      return (l : ls)

main :: IO ()
main = do
  first <- getLine
  print ("first line", first)
  rest <- readAllLines
  print ("remaining lines until EOF", rest)
  r <- catchIOError (fmap Right getLine) (return . Left)
  case r of
    Left e -> print ("getLine at EOF", isEOFError e, isUserError e, isDoesNotExistError e)
    Right l -> print ("unexpected line", l)
  again <- catchIOError (fmap Just getLine) (\_ -> return Nothing)
  print ("still at EOF", again)
  c <- catchIOError (fmap Just getChar) (\e -> if isEOFError e then return Nothing else ioError e)
  print ("getChar at EOF", c)
  putStrLn "done"
