{-# LANGUAGE BangPatterns #-}
-- BangPatterns: a binding marked strict, in a let or a where, is worked out before what the binding is for
module Main (main) where

mean :: [Double] -> Double
mean xs = total / count
  where
    !total = sum xs
    !count = fromIntegral (length xs)

sumTo :: Int -> Int
sumTo n = go 0 1
  where
    go acc i
      | i > n = acc
      | otherwise = let !acc' = acc + i in go acc' (i + 1)

pairs :: Int -> (Int, Int)
pairs n =
  let !(a, b) = (n, n + 1)
   in (b, a)

guarded :: Int -> String
guarded n
  | n > 0 = "positive"
  | otherwise = "not"
  where
    !_checked = if n == 13 then error "unlucky" else n

main :: IO ()
main = do
  print (mean [1, 2, 3, 4])
  print (sumTo 100000)
  print (pairs 1)
  print (map guarded [1, -1])
  let !x = 2 + 2 :: Int
      y = undefined :: Int
  print x
  -- A binding that is not marked is not worked out unless it is needed.
  print (let z = undefined :: Int in "lazy")
  print (let lazy = undefined :: Int; !strict = 1 :: Int in strict)
  print [v | u <- [1, 2 :: Int], let !v = u * 2]
  -- One that is marked is worked out though nothing uses it.
  let !w = undefined :: Int
  putStrLn "not reached"
