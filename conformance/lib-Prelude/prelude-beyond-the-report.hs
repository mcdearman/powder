{-# LANGUAGE GHC2021 #-}
-- The Prelude of the editions after Haskell 2010: what it has that the Report's does not, with nothing imported
data Pair a = Pair a a deriving Show

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Foldable Pair where
  foldr f z (Pair a b) = f a (f b z)

instance Traversable Pair where
  traverse f (Pair a b) = Pair <$> f a <*> f b

instance Applicative Pair where
  pure a = Pair a a
  Pair f g <*> Pair a b = Pair (f a) (g b)

newtype Longest = Longest String deriving Show

instance Semigroup Longest where
  Longest a <> Longest b = if length b > length a then Longest b else Longest a

instance Monoid Longest where
  mempty = Longest ""

newtype Checked a = Checked (Either String a) deriving Show

instance Functor Checked where
  fmap f (Checked e) = Checked (fmap f e)

instance Applicative Checked where
  pure = Checked . Right
  Checked f <*> Checked a = Checked (f <*> a)

instance Monad Checked where
  Checked (Left e) >>= _ = Checked (Left e)
  Checked (Right a) >>= k = k a

instance MonadFail Checked where
  fail _ = Checked (Left "no match")

main :: IO ()
main = do
  putStrLn ("functor: " ++ show ((+ 1) <$> Just (1 :: Int), 'x' <$ [1, 2 :: Int]))
  putStrLn ("applicative: " ++ show (pure 1 :: Maybe Int, (+) <$> Just 1 <*> Just (2 :: Int), Just 'a' *> Just 'b', Just 'a' <* Just 'b'))
  putStrLn ("liftA2: " ++ show (liftA2 (+) (Pair 1 2) (Pair 10 (20 :: Int))))
  putStrLn ("semigroup: " ++ show ("ab" <> "cd", Just [1 :: Int] <> Nothing, Longest "ab" <> Longest "abc"))
  putStrLn ("monoid: " ++ show (mempty :: String, mappend "a" "b", mconcat [Longest "a", Longest "abc", Longest "ab"]))
  putStrLn ("foldable: " ++ show (length (Pair 'a' 'b'), sum (Pair 1 (2 :: Int)), elem 2 (Pair 1 (2 :: Int)), maximum (Pair 'a' 'z'), null (Pair () ())))
  putStrLn ("foldable-folds: " ++ show (foldr (:) [] (Pair 1 (2 :: Int)), foldl (-) 10 (Pair 1 (2 :: Int)), foldMap show (Pair 1 (2 :: Int)), foldr1 (-) (Pair 5 (3 :: Int))))
  putStrLn ("foldable-functions: " ++ show (any even (Just (2 :: Int)), all even (Pair 1 (2 :: Int)), concatMap show (Just (12 :: Int)), and (Pair True False), notElem 'c' (Pair 'a' 'b')))
  putStrLn ("traversable: " ++ show (traverse (\x -> if x > 0 then Just x else Nothing) (Pair 1 (2 :: Int)), sequenceA [Just 'a', Nothing]))
  putStrLn ("traversable-monadic: " ++ show (mapM (\x -> [x, x * 2]) (Pair 1 (2 :: Int)), sequence (Just "ab")))
  mapM_ (putStrLn . ("mapM_: " ++)) (Pair "first" "second")
  sequence_ (Just (putStrLn "sequence_: ran"))
  putStrLn ("monadfail: " ++ show (do { Just x <- Checked (Right (Nothing :: Maybe Int)); return x }))
  putStrLn ("monadfail-direct: " ++ show (fail "x" :: Maybe Int))
