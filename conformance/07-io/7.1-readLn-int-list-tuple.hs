-- Haskell 2010 Report 7.1: readLn = getLine >>= readIO, at Int, [Int], a tuple, Double, Maybe, String and Char
sumLines :: Int -> Int -> IO Int
sumLines 0 acc = return acc
sumLines k acc = do
  v <- readLn
  sumLines (k - 1) (acc + v)

main :: IO ()
main = do
  n <- readLn :: IO Int
  print ("Int", n, n + 1)
  xs <- readLn :: IO [Int]
  print ("[Int]", xs, sum xs)
  t <- readLn :: IO (Int, String)
  print ("tuple", t, fst t * 2, length (snd t))
  d <- readLn :: IO Double
  print ("Double with surrounding spaces", d, d * 2)
  m <- readLn :: IO (Maybe Bool)
  print ("Maybe Bool", m)
  s <- readLn :: IO String
  print ("String needs quotes in the input", s, length s)
  c <- readLn :: IO Char
  print ("Char", c)
  neg <- readLn :: IO Integer
  print ("negative Integer", neg, neg * neg)
  nested <- readLn :: IO [(Int, [Bool])]
  print ("nested", nested)
  u <- readLn :: IO ()
  print ("unit", u)
  total <- sumLines 3 0
  print ("three more numbers summed", total)
