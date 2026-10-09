-- Haskell 2010 Report 7.2: sequence_, mapM_, sequence, mapM over I/O actions; loops by recursion in IO
import Control.Monad (forM_, unless, when)

countdown :: Int -> IO ()
countdown 0 = putStrLn "liftoff"
countdown n = do
  print n
  countdown (n - 1)

sumTo :: Int -> Int -> IO Int
sumTo acc 0 = return acc
sumTo acc n = sumTo (acc + n) (n - 1)

collatz :: Int -> IO Int
collatz 1 = return 0
collatz n = do
  steps <- collatz (if even n then n `div` 2 else 3 * n + 1)
  return (steps + 1)

triangle :: Int -> IO ()
triangle n = mapM_ (\k -> putStrLn (replicate k '*')) [1 .. n]

main :: IO ()
main = do
  sequence_ [putStr "a", putStr "b", putStrLn "c"]
  sequence_ []
  mapM_ print [1, 2, 3 :: Int]
  mapM_ putStrLn []
  mapM_ (putStrLn . ("item " ++) . show) [10, 20 :: Int]
  rs <- sequence [return 1, putStrLn "effect inside sequence" >> return 2, return (3 :: Int)]
  print ("sequence collects", rs)
  ls <- mapM (\s -> putStrLn ("visit " ++ s) >> return (length s)) ["x", "yy", "zzz"]
  print ("mapM collects", ls)
  unitResult <- mapM_ return [1, 2, 3 :: Int]
  print ("mapM_ result", unitResult)
  countdown 3
  total <- sumTo 0 100000
  print ("deep recursion in IO", total)
  c <- collatz 27
  print ("collatz steps for 27", c)
  triangle 4
  forM_ [1, 2 :: Int] (\i -> forM_ "ab" (\ch -> putStrLn (show i ++ [ch])))
  when (c > 100) (putStrLn "when: condition true")
  when (c < 100) (putStrLn "when: must not print")
  unless (c > 100) (putStrLn "unless: must not print")
  unless (c < 100) (putStrLn "unless: condition false")
  mapM_ (\(i, s) -> putStrLn (show i ++ ". " ++ s)) (zip [(1 :: Int) ..] ["alpha", "beta", "gamma"])
  sequence_ (replicate 3 (putStr "ab"))
  putStrLn ""
  foldr (\x rest -> print x >> rest) (putStrLn "end of foldr") "xyz"
  r <- foldl (\acc x -> acc >>= \a -> return (a * 10 + x)) (return 0) [1, 2, 3 :: Int]
  print ("fold of binds", r)
