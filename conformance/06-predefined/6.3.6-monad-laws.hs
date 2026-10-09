-- Haskell 2010 Report 6.3.6: the monad laws (left identity, right identity, associativity) and fmap f xs == xs >>= return . f
f :: Int -> Maybe Int
f x = if x > 0 then Just (x * 2) else Nothing

g :: Int -> Maybe Int
g x = if x < 100 then Just (x + 1) else Nothing

fl :: Int -> [Int]
fl x = [x, x + 1]

gl :: Int -> [Int]
gl x = replicate (x `mod` 3) x

fio :: Int -> IO Int
fio x = return (x * 2)

gio :: Int -> IO Int
gio x = return (x + 1)

maybes :: [Maybe Int]
maybes = [Nothing, Just (-1), Just 3, Just 60, Just 200]

main :: IO ()
main = do
  print ("Maybe left identity", and [(return a >>= f) == f a | a <- [-1, 0, 5, 70]])
  print ("Maybe right identity", and [(m >>= return) == m | m <- maybes])
  print ("Maybe associativity", and [((m >>= f) >>= g) == (m >>= (\x -> f x >>= g)) | m <- maybes])
  print ("Maybe fmap via bind", and [fmap (+ 1) m == (m >>= return . (+ 1)) | m <- maybes])
  print ("Maybe then via bind", and [(m >> Just 'k') == (m >>= \_ -> Just 'k') | m <- maybes])
  print ("list left identity", and [(return a >>= fl) == fl a | a <- [0, 1, 2]])
  print ("list right identity", and [(m >>= return) == m | m <- [[], [1], [1, 2, 3 :: Int]]])
  print ("list associativity", and [((m >>= fl) >>= gl) == (m >>= (\x -> fl x >>= gl)) | m <- [[], [1], [1, 2, 3], [4, 5, 6, 7]]])
  print ("list fmap via bind", fmap (* 2) [1, 2, 3 :: Int] == ([1, 2, 3] >>= return . (* 2)))
  print ("list bind is concatMap", ([1, 2, 3] >>= fl) == concatMap fl [1, 2, 3], ([1, 2, 3] >>= fl) == concat (map fl [1, 2, 3]))
  a1 <- return 7 >>= fio
  a2 <- fio 7
  print ("IO left identity", a1 == a2, a1)
  b1 <- fio 7 >>= return
  print ("IO right identity", b1 == a2)
  c1 <- (fio 7 >>= gio) >>= fio
  c2 <- fio 7 >>= (\x -> gio x >>= fio)
  print ("IO associativity", c1 == c2, c1)
  d1 <- fmap (+ 1) (fio 7)
  d2 <- fio 7 >>= return . (+ 1)
  print ("IO fmap via bind", d1 == d2, d1)
  print ("do block equals desugaring", (do { x <- Just 3; y <- f x; return (x + y) }) == (Just 3 >>= \x -> f x >>= \y -> return (x + y)))
