{-# LANGUAGE GHC2021 #-}
-- Data.Foldable: instances written by the program, one giving foldMap and one giving foldr, and every method from each
import Data.Foldable

data Tree a = Leaf | Node (Tree a) a (Tree a)

instance Foldable Tree where
  foldMap _ Leaf = mempty
  foldMap f (Node l x r) = foldMap f l <> f x <> foldMap f r

data Rose a = Rose a [Rose a]

instance Foldable Rose where
  foldr f z (Rose x kids) = f x (foldr (\k acc -> foldr f acc k) z kids)

fromList :: Ord a => [a] -> Tree a
fromList = foldr insert Leaf
  where
    insert x Leaf = Node Leaf x Leaf
    insert x t@(Node l y r)
      | x < y = Node (insert x l) y r
      | x > y = Node l y (insert x r)
      | otherwise = t

main :: IO ()
main = do
  let t = fromList [5, 2, 8, 1, 9, 3 :: Int]
      r = Rose 1 [Rose 2 [Rose 4 []], Rose (3 :: Int) []]
  putStrLn ("tree-toList: " ++ show (toList t))
  putStrLn ("tree-size: " ++ show (length t, null t, null (Leaf :: Tree Int)))
  putStrLn ("tree-numbers: " ++ show (sum t, product t, maximum t, minimum t))
  putStrLn ("tree-elem: " ++ show (elem 8 t, elem 7 t, notElem 7 t))
  putStrLn ("tree-foldr: " ++ show (foldr (:) [] t, foldr (-) 0 t))
  putStrLn ("tree-foldl: " ++ show (foldl (flip (:)) [] t, foldl (-) 0 t))
  putStrLn ("tree-fold1: " ++ show (foldr1 (-) t, foldl1 (-) t))
  putStrLn ("tree-strict: " ++ show (foldl' (+) 0 t, foldr' (+) 0 t))
  putStrLn ("tree-foldMap: " ++ show (foldMap (\x -> [x, x * 10]) t))
  putStrLn ("tree-fold: " ++ show (fold (Node Leaf "ab" (Node Leaf "cd" Leaf))))
  putStrLn ("rose-toList: " ++ show (toList r))
  putStrLn ("rose-size: " ++ show (length r, null r))
  putStrLn ("rose-numbers: " ++ show (sum r, product r, maximum r, minimum r))
  putStrLn ("rose-elem: " ++ show (elem 4 r, elem 5 r))
  putStrLn ("rose-foldl: " ++ show (foldl (flip (:)) [] r))
  putStrLn ("rose-foldMap: " ++ show (foldMap show r))
  putStrLn ("searching: " ++ show (any even t, all (> 0) t, find (> 4) t, find (> 9) t))
  putStrLn ("joining: " ++ show (concatMap show t, concat (Node Leaf [1 :: Int] (Node Leaf [2, 3] Leaf))))
  putStrLn ("booleans: " ++ show (and (Node Leaf True Leaf), or (Node Leaf False Leaf)))
  putStrLn ("by: " ++ show (maximumBy (\a b -> compare (a `mod` 5) (b `mod` 5)) t, minimumBy (\a b -> compare (a `mod` 5) (b `mod` 5)) t))
  putStrLn ("lazy: " ++ show (foldr (\x _ -> x) 0 (Node undefined (7 :: Int) undefined `seq` Rose (1 :: Int) undefined)))
