-- Haskell 2010 Report 6.2: ($!) is strict application, f $! x = x `seq` f x; ($) and ($!) are infixr 0
data Lazy = Lazy Int Int

data Mixed = Mixed !Int Int

lazyFirst :: Lazy -> Int
lazyFirst (Lazy a _) = a

mixedFirst :: Mixed -> Int
mixedFirst (Mixed a _) = a

bottomInt :: Int
bottomInt = undefined

len :: [a] -> Int
len = go 0
  where
    go :: Int -> [a] -> Int
    go n [] = n
    go n (_ : xs) = (go $! n + 1) xs

main :: IO ()
main = do
  print ("$ applies", negate $ (3 :: Int), succ $ 'a')
  print ("$! applies", negate $! (3 :: Int), succ $! 'a')
  print ("$ is lazy", const 'k' $ bottomInt)
  print ("$! to WHNF only", const 'k' $! (bottomInt, bottomInt), const 'k' $! Just bottomInt)
  print ("$! with function argument", const 'k' $! (\x -> x + bottomInt))
  print ("$ right assoc", negate $ abs $ subtract 10 $ (3 :: Int))
  print ("$! right assoc", negate $! abs $! subtract 10 $! (3 :: Int))
  print ("mixed chain", succ $ succ $! pred $ (10 :: Int))
  print ("precedence 0 below +", succ $! 1 + 2 * (3 :: Int))
  print ("precedence 0 below ++", length $! "ab" ++ "cd")
  print ("sections", map ($ 3) [(+ 1), (* 2), subtract (1 :: Int)], map ($! 3) [(+ 1), (* 2), subtract (1 :: Int)])
  print ("as functions", ($) succ 'a', ($!) succ 'a', zipWith ($!) [not, id] [True, True])
  print ("strict loop", len "hello", len [(1 :: Int) .. 5000])
  print ("lazy constructor keeps bottom unevaluated", lazyFirst (Lazy 1 bottomInt))
  print ("strict field evaluated, lazy field not", mixedFirst (Mixed 2 bottomInt))
  print ("seq of constructor with lazy bottom field", seq (Mixed 3 bottomInt) "ok")
  print ("strict constructor via $!", mixedFirst (Mixed 4 $! 5), mixedFirst ((Mixed $! 6) bottomInt))
  print ("partial strict constructor is a function", seq (Mixed bottomInt) "ok")
  print ("foldl with $!", foldl (\acc x -> ((+) $! acc) x) 0 [1 .. 100 :: Int])
