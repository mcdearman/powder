-- Haskell 2010 Report 7.1: repeated getLine until a sentinel line; input after the sentinel is left unread
readNumbers :: IO [Int]
readNumbers = do
  l <- getLine
  if l == "END"
    then return []
    else do
      ns <- readNumbers
      return (read l : ns)

echoUntilBlank :: Int -> IO Int
echoUntilBlank n = do
  l <- getLine
  if null l
    then return n
    else do
      putStrLn (show n ++ ": " ++ l)
      echoUntilBlank (n + 1)

main :: IO ()
main = do
  ns <- readNumbers
  print ("numbers", ns)
  print ("sum", sum ns)
  count <- echoUntilBlank 1
  print ("lines echoed", count - 1)
  next <- getLine
  print ("line after the blank", next)
