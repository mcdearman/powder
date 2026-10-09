-- Haskell 2010 Report 9 (and 6.2): seq, ($!), error, undefined; bottoms here are never forced
module Main (main) where

bottomInt :: Int
bottomInt = error "never evaluated"

sumStrict :: Int -> [Int] -> Int
sumStrict acc [] = acc
sumStrict acc (x : xs) = let acc' = acc + x in acc' `seq` sumStrict acc' xs

countStrict :: Int -> Int -> Int
countStrict acc 0 = acc
countStrict acc n = (countStrict $! acc + 1) (n - 1)

main :: IO ()
main = do
  print ("seq returns second", seq (1 :: Int) 'a', (2 :: Int) `seq` "b")
  print ("seq forces only to WHNF: pair", (undefined :: Int, undefined :: Int) `seq` 1)
  print ("seq forces only to WHNF: cons", ((undefined :: Int) : undefined) `seq` 2)
  print ("seq forces only to WHNF: Just", Just (undefined :: Int) `seq` 3)
  print ("seq forces only to WHNF: lambda", (\x -> (undefined :: Int) + x) `seq` 4)
  print ("seq on partial application", const (undefined :: Int) `seq` 5)
  print ("seq on constructor function", (Just :: Int -> Maybe Int) `seq` 6)
  print ("seq chain", (1 :: Int) `seq` (2 :: Int) `seq` (3 :: Int))
  print ("seq strict fold", sumStrict 0 [1 .. 100000])
  print ("$! applies", succ $! (1 :: Int), (+ 1) $! 2 * (3 :: Int))
  print ("$! to WHNF only", length $! [undefined :: Int, undefined], fst $! (1 :: Int, undefined :: Int))
  print ("$! const", const 1 $! (2 :: Int))
  print ("$! strict loop", countStrict 0 100000)
  print ("$! = $ for defined values", (negate $! (5 :: Int)) == (negate $ 5))
  print ("error unevaluated in data", length [bottomInt, error "x", undefined])
  print ("error unevaluated argument", const 'k' (error "not forced" :: Int))
  print ("error unevaluated branch", if True then 1 else error "other branch" :: Int)
  print ("undefined at any type", fst (0 :: Int, undefined :: [Bool]), snd (undefined :: IO (), 'u'))
  print ("undefined in let", let u = undefined :: Int in 5 :: Int)
  print ("undefined in lazy pattern", (\ ~(a, b) -> 9 :: Int) (undefined :: (Int, Int)))
  print ("error has any type", null [error "a" :: Int -> Int], take 0 (error "list" :: String))
