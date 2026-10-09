-- Haskell 2010 Report 9: mapM, mapM_, sequence, sequence_, (=<<) in the IO, Maybe and list monads
module Main (main) where

half :: Int -> Maybe Int
half n = if even n then Just (n `div` 2) else Nothing

shout :: Int -> IO Int
shout n = do
  putStrLn ("visit " ++ show n)
  return (n * n)

main :: IO ()
main = do
  xs <- mapM shout [1, 2, 3]
  print ("mapM IO", xs)
  ys <- mapM shout []
  print ("mapM IO empty", ys)
  mapM_ (putStrLn . ("line " ++) . show) [1, 2, 3 :: Int]
  u <- mapM_ shout [7]
  print ("mapM_ IO result", u)
  mapM_ print ([] :: [Int])
  zs <- sequence [return 1, shout 4, return (3 :: Int)]
  print ("sequence IO", zs)
  sequence_ [putStr "a", putStr "b", putStrLn "c"]
  v <- sequence_ ([] :: [IO Int])
  print ("sequence_ IO empty", v)
  w <- shout =<< return 5
  print ("=<< IO", w)
  print =<< mapM (return . (+ 1)) [1, 2, 3 :: Int]
  print ("mapM Maybe", mapM half [2, 4, 6], mapM half [2, 3, 6], mapM half [])
  print ("mapM_ Maybe", mapM_ half [2, 4], mapM_ half [2, 3], mapM_ half [])
  print ("sequence Maybe", sequence [Just 1, Just (2 :: Int)], sequence [Just 1, Nothing, Just (3 :: Int)], sequence ([] :: [Maybe Int]))
  print ("sequence_ Maybe", sequence_ [Just 1, Just (2 :: Int)], sequence_ [Just (1 :: Int), Nothing])
  print ("=<< Maybe", half =<< Just 10, half =<< Just 5, half =<< Nothing, half =<< half =<< Just 12)
  print ("mapM list", mapM (\x -> [x, x * 10]) [1, 2 :: Int])
  print ("mapM list empty", mapM (\x -> [x, x]) ([] :: [Int]), mapM (const ([] :: [Int])) [1, 2 :: Int])
  print ("mapM_ list", mapM_ (\x -> [x, x]) [1, 2 :: Int])
  print ("sequence list", sequence [[1, 2], [3], [4, 5 :: Int]], sequence ["ab", "cd"])
  print ("sequence list edge", sequence ([] :: [[Int]]), sequence [[1, 2], [], [3 :: Int]])
  print ("sequence_ list", sequence_ ["ab", "cd"], sequence_ ["ab", ""])
  print ("=<< list", (\x -> [x, x + 1]) =<< [10, 20 :: Int], replicate 2 =<< "abc")
  print ("=<< is flip >>=", (half =<< Just 8) == (Just 8 >>= half))
  print ("sequence = mapM id", sequence [Just 'a', Just 'b'] == mapM id [Just 'a', Just 'b'])
