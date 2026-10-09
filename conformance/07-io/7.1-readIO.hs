-- Haskell 2010 Report 7.1: readIO parses a string in the IO monad (like read, but failure is an I/O error, tested in 7.3)
parseAll :: [String] -> IO [Int]
parseAll = mapM readIO

main :: IO ()
main = do
  n <- readIO "12" :: IO Int
  print ("Int", n)
  xs <- readIO " [1,2,3] " :: IO [Int]
  print ("list with surrounding spaces", xs)
  p <- readIO "(True,'x')" :: IO (Bool, Char)
  print ("pair", p)
  d <- readIO "2.5e-1" :: IO Double
  print ("Double", d)
  m <- readIO "Just (-3)" :: IO (Maybe Int)
  print ("Maybe", m)
  o <- readIO "(GT)" :: IO Ordering
  print ("parenthesised", o)
  s <- readIO (show "round trip\n") :: IO String
  print ("String", s)
  ns <- parseAll ["1", " 2", "3 ", "(4)"]
  print ("mapM readIO", ns, sum ns)
  k <- readIO "40" >>= \v -> return (v + 2 :: Int)
  print ("bind", k)
  big <- readIO "123456789012345678901234567890" :: IO Integer
  print ("Integer", big + 1)
