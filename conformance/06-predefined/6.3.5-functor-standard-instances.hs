-- Haskell 2010 Report 6.3.5: class Functor; fmap at lists, Maybe and IO, and the functor laws on sample values
double :: Int -> Int
double = (* 2)

inc :: Int -> Int
inc = (+ 1)

bump :: Functor f => f Int -> f Int
bump = fmap inc

main :: IO ()
main = do
  print ("list", fmap inc [1, 2, 3], fmap show [True, False], fmap inc [])
  print ("list fmap is map", fmap double [1, 2, 3] == map double [1, 2, 3])
  print ("Maybe", fmap inc (Just 1), fmap inc Nothing, fmap length (Just "abc"))
  print ("changes element type", fmap even [1, 2 :: Int], fmap fromEnum (Just 'a'), fmap (: []) "ab")
  print ("nested", fmap (fmap inc) [Just 1, Nothing, Just 3], fmap (fmap inc) (Just [1, 2]))
  x <- fmap inc (return 41)
  print ("IO", x)
  n <- fmap length (return "hello")
  print ("IO changes type", n)
  ys <- fmap (map double) (return [1, 2, 3])
  print ("IO of list", ys)
  u <- fmap (const 'u') (putStr "")
  print ("IO action still runs once, result replaced", u)
  print ("identity law list", fmap id [1, 2, 3 :: Int] == id [1, 2, 3])
  print ("identity law Maybe", fmap id (Just 'a') == id (Just 'a'), fmap id (Nothing :: Maybe Char) == Nothing)
  print ("composition law list", fmap (double . inc) [1, 2, 3] == (fmap double . fmap inc) [1, 2, 3])
  print ("composition law Maybe", fmap (double . inc) (Just 5) == (fmap double . fmap inc) (Just 5))
  a <- fmap (double . inc) (return 5)
  b <- (fmap double . fmap inc) (return 5)
  print ("composition law IO", a == b, a)
  print ("overloaded", bump [1, 2], bump (Just 9), bump Nothing)
  z <- bump (return 99)
  print ("overloaded IO", z)
  print ("fmap as a function", map (fmap inc) [Just 1, Nothing], (fmap . fmap) inc [[1], [2, 3]])
  print ("lazy", take 3 (fmap double [1 ..]), fmap (const 'k') (Just (undefined :: Int)))
