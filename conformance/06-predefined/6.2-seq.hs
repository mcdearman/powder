-- Haskell 2010 Report 6.2: seq evaluates its first argument to weak head normal form only
data Pair = Pair Int Int

data Wrap a = Wrap a

newtype New = New Int

add :: Int -> Int -> Int
add x y = x + y

bottomInt :: Int
bottomInt = undefined

sumStrict :: Int -> [Int] -> Int
sumStrict acc [] = acc
sumStrict acc (x : xs) = let acc' = acc + x in acc' `seq` sumStrict acc' xs

useSeq :: Int -> Int
useSeq n = let unused = n * 2 in unused `seq` n

main :: IO ()
main = do
  print ("seq a b = b", seq (1 :: Int) 'b', seq "x" (2 :: Int), 'a' `seq` True)
  print ("const ignores bottom", const (1 :: Int) (undefined :: Char))
  print ("tuple with bottom fields", seq (bottomInt, bottomInt) "ok")
  print ("Just bottom", seq (Just bottomInt) "ok")
  print ("cons of bottoms", seq (bottomInt : undefined) "ok")
  print ("user constructor", seq (Pair bottomInt bottomInt) "ok", seq (Wrap bottomInt) "ok")
  print ("lambda", seq (\x -> x + bottomInt) "ok")
  print ("lambda with bottom body", seq (\_ -> bottomInt) "ok")
  print ("partial application", seq (add bottomInt) "ok", seq (const bottomInt :: Char -> Int) "ok")
  print ("constructor as function", seq (Pair bottomInt) "ok", seq Just "ok")
  print ("string with bottom tail", seq ('a' : undefined) "ok")
  print ("nested seq", seq (seq 'a' 'b') 'c', seq 'a' (seq 'b' 'c'))
  print ("second argument returned lazily", fst (seq 'a' ('x', bottomInt)))
  print ("seq is a function", map (seq 'z') [1, 2 :: Int], zipWith seq "ab" [True, False])
  print ("infix, right assoc, prec 0", 'a' `seq` 'b' `seq` 1 + 2 :: Int)
  print ("strict accumulator", sumStrict 0 [1 .. 1000])
  print ("seq on IO action does not run it", seq (putStrLn "must not be printed") "ok")
  print ("seq on a list forces only the first cell", seq [bottomInt, bottomInt] "ok")
  print ("newtype of defined value", seq (New 3) "ok")
  print ("seq of an unused local", useSeq 5)
