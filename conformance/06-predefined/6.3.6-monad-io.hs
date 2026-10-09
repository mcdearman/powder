-- Haskell 2010 Report 6.3.6: the IO instance of Monad; (>>=), (>>), return, mapM, mapM_, sequence, sequence_, (=<<)
announce :: String -> IO Int
announce s = putStrLn ("run " ++ s) >> return (length s)

countdown :: Int -> IO ()
countdown 0 = putStrLn "liftoff"
countdown n = print n >> countdown (n - 1)

main :: IO ()
main = do
  return 5 >>= print
  return "bound" >>= putStrLn
  putStrLn "first" >> putStrLn "second"
  n <- announce "abc" >>= \k -> return (k * 2)
  print ("bind result", n)
  m <- announce "x" >> announce "yz"
  print ("then keeps second result", m)
  r <- return 'r'
  print ("return", r)
  () <- return ()
  ks <- mapM announce ["a", "bb", "ccc"]
  print ("mapM results in order", ks)
  mapM_ print [1, 2, 3 :: Int]
  rs <- sequence [announce "p", return 0, announce "qq"]
  print ("sequence results", rs)
  sequence_ [putStr "a", putStr "b", putStrLn "c"]
  u <- mapM_ announce ["only effects"]
  print ("mapM_ returns unit", u)
  es <- mapM announce []
  print ("mapM on empty", es)
  print =<< announce "flipped bind"
  countdown 3
  v <- return 1 >>= (\a -> return (a + 1) >>= (\b -> return (a + b :: Int)))
  w <- (return 1 >>= \a -> return (a + 1)) >>= \b -> return (b + b :: Int)
  print ("nested binds", v, w)
  let unused = announce "never sequenced"
  x <- return 3 >>= \a -> let b = a * a in return (b + 1 :: Int)
  print ("let in bind", x)
  y <- unused `seq` return 'y'
  print ("seq on an action does not run it", y)
