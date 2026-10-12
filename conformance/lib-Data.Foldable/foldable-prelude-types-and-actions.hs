-- Data.Foldable: the instances for lists, Maybe, Either, pairs and arrays, and the folds that run actions
import Data.Array
import Data.Foldable
import qualified Data.List as L

main :: IO ()
main = do
  let a = listArray (0, 4) [10, 20, 30, 40, 50 :: Int]
  putStrLn ("list: " ++ show (length "abc", sum [1, 2, 3 :: Int], elem 'b' "abc", maximum "hello", toList [1, 2 :: Int]))
  putStrLn ("list-long: " ++ show (L.foldl' (+) 0 [1 .. 100000 :: Int], length [1 .. 100000 :: Int]))
  putStrLn ("maybe: " ++ show (length (Just 'x'), length (Nothing :: Maybe Char), sum (Just (3 :: Int)), toList (Just 'x'), null (Just 'x')))
  putStrLn ("maybe-search: " ++ show (elem 3 (Just (3 :: Int)), concat (Just [1, 2 :: Int]), maximum (Just 'm')))
  putStrLn ("either: " ++ show (length (Left 'a' :: Either Char Int), sum (Right 3 :: Either Char Int), toList (Left 'a' :: Either Char Int)))
  putStrLn ("pair: " ++ show (length (1 :: Int, 'x'), toList ('a', 'b'), sum ("ignored", 5 :: Int)))
  putStrLn ("array: " ++ show (sum a, length a, maximum a, minimum a, elem 30 a, toList a))
  putStrLn ("array-folds: " ++ show (foldr (:) [] a, foldl (-) 0 a, foldMap show a))
  putStrLn ("list-functions: " ++ show (L.find even [1, 3, 4, 5 :: Int], L.maximumBy (\x y -> compare (snd x) (snd y)) [(1 :: Int, 'a'), (2, 'c'), (3, 'b')]))
  mapM_ (putStrLn . ("mapM_-maybe: " ++)) (Just "once")
  mapM_ (putStrLn . ("mapM_-nothing: " ++)) Nothing
  forM_ a (\x -> putStrLn ("forM_-array: " ++ show x))
  for_ [1, 2 :: Int] (\x -> putStrLn ("for_: " ++ show x))
  traverse_ (putStrLn . ("traverse_-either: " ++) . show) (Right 3 :: Either String Int)
  sequence_ [putStr "sequence_: a", putStr "b", putStrLn "c"]
  sequenceA_ (Just (putStrLn "sequenceA_: ran"))
  putStrLn ("traverse_-maybe: " ++ show (traverse_ (\x -> if x > 0 then Just x else Nothing) [1, 2, 3 :: Int], traverse_ (\x -> if x > 0 then Just x else Nothing) [1, 0, 3 :: Int]))
  putStrLn ("foldrM: " ++ show (foldrM (\x acc -> if x > 0 then Just (x + acc) else Nothing) 0 [1, 2, 3 :: Int]))
  putStrLn ("foldlM: " ++ show (foldlM (\acc x -> if x > 0 then Just (acc - x) else Nothing) 0 [1, 2, 3 :: Int], foldlM (\acc x -> if x > 0 then Just (acc - x) else Nothing) 0 [1, 0 :: Int]))
  r <- foldlM (\acc x -> putStrLn ("foldlM-io: " ++ show x) >> return (acc + x)) 0 [1, 2, 3 :: Int]
  putStrLn ("foldlM-io-result: " ++ show r)
  putStrLn ("asum: " ++ show (asum [Nothing, Just 1, Just (2 :: Int)], asum [[1], [2, 3 :: Int]]))
  putStrLn ("msum: " ++ show (msum (Just [1, 2 :: Int])))
