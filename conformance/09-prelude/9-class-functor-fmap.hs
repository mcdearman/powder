-- Haskell 2010 Report 9: class Functor (fmap) at list, Maybe and IO, plus a user instance
module Main (main) where

data Tree a = Leaf | Node (Tree a) a (Tree a)

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

toList :: Tree a -> [a]
toList Leaf = []
toList (Node l x r) = toList l ++ [x] ++ toList r

sample :: Tree Int
sample = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

main :: IO ()
main = do
  print ("list", fmap (+ 1) [1, 2, 3 :: Int], fmap show [1, 2 :: Int], fmap not ([] :: [Bool]))
  print ("list = map", fmap (* 2) [1, 2, 3 :: Int] == map (* 2) [1, 2, 3])
  print ("list infinite", take 3 (fmap (* 2) [1 :: Int ..]))
  print ("Maybe", fmap (+ 1) (Just 1 :: Maybe Int), fmap (+ 1) (Nothing :: Maybe Int), fmap length (Just "abc"))
  print ("Maybe changes type", fmap show (Just (5 :: Int)), fmap (const ()) (Just 'x'))
  n <- fmap (* 2) (return 21 :: IO Int)
  print ("IO", n)
  s <- fmap length (return "four")
  print ("IO changes type", s)
  u <- fmap (const 'k') (putStrLn "effect still happens")
  print ("IO effect", u)
  print ("identity law", fmap id [1, 2, 3 :: Int] == [1, 2, 3], fmap id (Just 'a') == Just 'a')
  print ("composition law", fmap (show . (+ 1)) [1, 2 :: Int] == (fmap show . fmap (+ 1)) [1, 2 :: Int])
  print ("user instance", toList (fmap (* 10) sample), toList (fmap show sample))
  print ("user instance empty", toList (fmap negate (Leaf :: Tree Int)))
  print ("nested", fmap (fmap (+ 1)) [Just 1, Nothing, Just (3 :: Int)], fmap (fmap succ) (Just "abc"))
  print ("lazy", length (fmap undefined [1, 2, 3 :: Int] :: [Int]), fmap fst (Just (1 :: Int, undefined :: Int)))
