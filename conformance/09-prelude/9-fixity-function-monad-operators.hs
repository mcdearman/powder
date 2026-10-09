-- Haskell 2010 Report 9 (and 4.4.2): infixr 9 .; infixl 1 >> >>=; infixr 1 =<<; infixr 0 $ $! `seq`
-- Every expression below either changes value or fails to typecheck if a fixity is wrong.
module Main (main) where

inc :: Int -> Int
inc = (+ 1)

dbl :: Int -> Int
dbl = (* 2)

half :: Int -> Maybe Int
half n = if even n then Just (n `div` 2) else Nothing

main :: IO ()
main = do
  print (". above $", inc . dbl $ 5, show . inc . dbl $ 5)
  print (". is infixr 9 (types force grouping)", length . show . inc $ 99, not . null . show $ ())
  print (". above ++ only via parens", (show . inc) 1 ++ (show . dbl) 1)
  print ("application above .", inc . max 3 $ 1, map inc . filter even $ [1, 2, 3, 4])
  print ("$ is infixr 0", negate $ abs $ inc $ dbl 3, inc $ dbl $ inc 1)
  print ("$ below +", negate $ 2 + 3 :: Int, (negate $ 2) + 3 :: Int)
  print ("$ below &&", not $ True && False, (not $ True) && False)
  print ("$ below ==", not $ 1 == (2 :: Int), show $ 2 * 3 == (6 :: Int))
  print ("$ below ++", length $ "ab" ++ "cd", reverse $ [1] ++ [2, 3 :: Int])
  print ("$ below >>=", show $ Just 8 >>= half >>= half)
  print ("$! is infixr 0", inc $! 2 * 3, negate $! inc $! dbl 5, (inc $! 2) * 3)
  print ("$ and $! mix", inc $ dbl $! 1 + 1, inc $! dbl $ 1 + 1)
  print ("seq is infixr 0", (1 :: Int) `seq` 2 + 3 :: Int, 'a' `seq` 'b' `seq` 'c')
  print ("seq below ==", () `seq` 1 == (1 :: Int), () `seq` True && False)
  print ("seq and $ mix", inc $ () `seq` 1 + 1, () `seq` inc $ 4)
  print (">>= is infixl 1", Just 12 >>= half >>= half, Just 6 >>= half >>= half)
  print (">>= with composed functions", Just 3 >>= Just . inc >>= Just . dbl, [1, 2] >>= replicate 2 >>= return . inc)
  print (">> and >>= same level", Just 'x' >> Just 8 >>= half, Just 8 >>= half >> Just 'y')
  print (">> is infixl", [1, 2 :: Int] >> "ab" >> [True])
  print (">>= below ||", Just (False || True) >>= return . not, Just 1 >>= \x -> Just (x + 1 == (2 :: Int) || False))
  print ("=<< is infixr 1", half =<< half =<< Just 12, Just . dbl =<< Just . inc =<< Just 3)
  print ("=<< below .", return . inc =<< [1, 2, 3])
  print ("lambda extends right over >>=", Just 2 >>= \x -> Just 3 >>= \y -> Just (x * y :: Int))
  putStr "a" >> putStr "b" >> return 'c' >>= print
  print $ inc $ dbl $ 20
  print . inc . dbl $ 20
  return 5 >>= print . inc
  print =<< return . dbl =<< return 4
