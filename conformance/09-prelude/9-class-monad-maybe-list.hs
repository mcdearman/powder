-- Haskell 2010 Report 9: class Monad ((>>=) (>>) return fail) at Maybe and list
module Main (main) where

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv a b = Just (a `div` b)

firstOfPair :: Maybe [Int] -> Maybe Int
firstOfPair m = do
  [x, _] <- m
  return x

onlyJusts :: [Maybe Int] -> [Int]
onlyJusts ms = do
  Just x <- ms
  return x

pythag :: Int -> [(Int, Int, Int)]
pythag n = do
  a <- [1 .. n]
  b <- [a .. n]
  c <- [b .. n]
  if a * a + b * b == c * c then return (a, b, c) else []

main :: IO ()
main = do
  print ("Maybe >>=", Just 10 >>= safeDiv 100, Just 0 >>= safeDiv 100, Nothing >>= safeDiv 100)
  print ("Maybe >>= chain", Just 100 >>= safeDiv 1000 >>= safeDiv 50, Just 0 >>= safeDiv 1 >>= safeDiv 50)
  print ("Maybe >>", Just 1 >> Just 'a', (Nothing :: Maybe Int) >> Just 'a', Just 1 >> (Nothing :: Maybe Char))
  print ("Maybe return", return 3 :: Maybe Int, (return 3 >>= safeDiv 9) == safeDiv 9 3)
  print ("Maybe fail", fail "ignored" :: Maybe Int, Just 1 >> (fail "x" :: Maybe Int))
  print ("Maybe do", do { x <- Just 3; y <- Just 4; return (x * y) } :: Maybe Int)
  print ("Maybe do Nothing", do { x <- Just 3; y <- Nothing; return (x * y) } :: Maybe Int)
  print ("Maybe pattern fail", firstOfPair (Just [1, 2]), firstOfPair (Just [1]), firstOfPair Nothing)
  print ("Maybe >> does not run rhs", (Nothing :: Maybe Int) >> (undefined :: Maybe Int))
  print ("list >>=", [1, 2, 3] >>= \x -> [x, x * 10 :: Int])
  print ("list >>= empty", ([] :: [Int]) >>= \x -> [x, x], [1, 2, 3 :: Int] >>= \_ -> ([] :: [Char]))
  print ("list >>", [1, 2 :: Int] >> "ab", ([] :: [Int]) >> "ab", [1, 2 :: Int] >> "")
  print ("list return", return 'x' :: String, return 1 :: [Int])
  print ("list fail", fail "ignored" :: [Int])
  print ("list do", do { x <- [1, 2]; y <- "ab"; return (x :: Int, y) })
  print ("list pattern fail", onlyJusts [Just 1, Nothing, Just 3, Nothing])
  print ("list search", pythag 20)
  print ("left identity", (return 4 >>= \x -> [x, x + 1]) == [4, 5 :: Int])
  print ("right identity", ([1, 2, 3 :: Int] >>= return) == [1, 2, 3])
  print ("associativity", (([1, 2 :: Int] >>= \x -> [x, x]) >>= \y -> [y + 1]) == ([1, 2] >>= \x -> [x, x] >>= \y -> [y + 1]))
  print ("infinite list bind", take 5 ([1 ..] >>= \x -> [x, negate x :: Int]))
