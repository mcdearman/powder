{-# LANGUAGE GHC2021 #-}
-- Data.Traversable: traverse, sequenceA, mapM, sequence, for and forM, over lists, Maybe, Either, pairs, arrays and a tree
import Data.Array
import Data.Foldable (toList)
import Data.Traversable

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
  foldr _ z Leaf = z
  foldr f z (Node l x r) = foldr f (f x (foldr f z r)) l

instance Traversable Tree where
  traverse _ Leaf = pure Leaf
  traverse f (Node l x r) = Node <$> traverse f l <*> f x <*> traverse f r

positive :: Int -> Maybe Int
positive x = if x > 0 then Just x else Nothing

main :: IO ()
main = do
  let t = Node (Node Leaf 1 Leaf) 2 (Node Leaf (3 :: Int) Leaf)
  putStrLn ("list-traverse: " ++ show (traverse positive [1, 2, 3], traverse positive [1, 0, 3]))
  putStrLn ("list-in-list: " ++ show (traverse (\x -> [x, x + 10]) [1, 2 :: Int]))
  putStrLn ("list-sequenceA: " ++ show (sequenceA [Just 1, Just (2 :: Int)], sequenceA [Just 1, Nothing :: Maybe Int]))
  putStrLn ("list-sequence: " ++ show (sequence [[1, 2], [3 :: Int]], sequence [Right 1, Left "stop", Left "late" :: Either String Int]))
  putStrLn ("list-mapM: " ++ show (mapM positive [3, 2, 1], mapM (\x -> Right x :: Either String Char) "abc"))
  putStrLn ("functions: " ++ show (sequenceA [(+ 1), (* 2), subtract 3] (10 :: Int)))
  putStrLn ("maybe: " ++ show (traverse (\x -> [x, x + 1]) (Just (1 :: Int)), traverse positive (Nothing :: Maybe Int), sequenceA (Just [1, 2 :: Int])))
  putStrLn ("either: " ++ show (traverse positive (Right 1 :: Either String Int), traverse positive (Left "l" :: Either String Int), sequenceA (Right "ab" :: Either Int String)))
  putStrLn ("pair: " ++ show (traverse positive ('k', 5), sequenceA ('k', [1, 2 :: Int])))
  putStrLn ("tree: " ++ show (traverse positive t))
  putStrLn ("tree-fails: " ++ show (traverse (\x -> if x == 2 then Nothing else Just x) t))
  putStrLn ("tree-lists: " ++ show (map toList (traverse (\x -> [x, negate x]) (Node Leaf 1 (Node Leaf (2 :: Int) Leaf)))))
  t' <- traverse (\x -> putStrLn ("tree-io: " ++ show x) >> return (x * 2)) t
  putStrLn ("tree-io-result: " ++ show (toList t'))
  putStrLn ("for: " ++ show (for [1, 2 :: Int] (\x -> Just (x + 1))))
  xs <- forM (Just 'q') (\c -> putStrLn ("forM-maybe: " ++ [c]) >> return (succ c))
  putStrLn ("forM-result: " ++ show xs)
  let a = listArray (1, 3) "abc"
  a' <- forM a (\c -> putStrLn ("forM-array: " ++ [c]) >> return (fromEnum c))
  putStrLn ("array: " ++ show (bounds a', elems a'))
  putStrLn ("array-maybe: " ++ show (fmap elems (traverse positive (listArray (0, 2) [1, 2, 3 :: Int])), fmap elems (traverse positive (listArray (0, 1) [1, 0 :: Int]))))
  putStrLn ("mapAccum: " ++ show (mapAccumL (\acc x -> (acc + x, acc * x)) 0 [1, 2, 3 :: Int], mapAccumR (\acc x -> (acc + x, acc * x)) 0 [1, 2, 3 :: Int]))
