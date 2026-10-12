-- Control.Applicative: the instances of Applicative for Maybe, lists, Either, functions, pairs and IO
import Control.Applicative

main :: IO ()
main = do
  putStrLn ("maybe-ap: " ++ show ((+) <$> Just 3 <*> Just (4 :: Int), (+) <$> Nothing <*> Just (4 :: Int)))
  putStrLn ("maybe-pure: " ++ show (pure 5 :: Maybe Int))
  putStrLn ("maybe-then: " ++ show (Just 1 *> Just (2 :: Int), Just (1 :: Int) <* Just 2, Nothing *> Just (2 :: Int)))
  putStrLn ("maybe-liftA2: " ++ show (liftA2 (,) (Just 'a') (Just True), liftA2 (,) (Just 'a') (Nothing :: Maybe Bool)))
  putStrLn ("list-ap: " ++ show ([(+ 1), (* 2)] <*> [10, 20 :: Int]))
  putStrLn ("list-liftA2: " ++ show (liftA2 (,) [1, 2 :: Int] "ab"))
  putStrLn ("list-then: " ++ show ([1, 2 :: Int] *> "ab", [1, 2 :: Int] <* "ab"))
  putStrLn ("either-ap: " ++ show (((+) <$> Right 1 <*> Right 2) :: Either String Int))
  putStrLn ("either-left: " ++ show (((+) <$> Left "first" <*> Left "second") :: Either String Int))
  putStrLn ("function-ap: " ++ show (((+) <$> (* 2) <*> (+ 10)) (3 :: Int)))
  putStrLn ("function-pure: " ++ show ((pure 'k' :: Int -> Char) 0))
  putStrLn ("pair-ap: " ++ show (("ab", (+ 1)) <*> ("cd", 2 :: Int)))
  putStrLn ("pair-pure: " ++ show (pure 'x' :: (String, Char)))
  s <- (++) <$> pure "io " <*> pure "works"
  putStrLn ("io-ap: " ++ s)
  n <- putStrLn "io-then: first" *> pure (1 :: Int) <* putStrLn "io-then: last"
  putStrLn ("io-then-result: " ++ show n)
  putStrLn ("liftA: " ++ show (liftA (+ 1) (Just (1 :: Int))))
  putStrLn ("liftA3: " ++ show (liftA3 (,,) (Just 'a') (Just 'b') (Just 'c')))
  putStrLn ("flipped-ap: " ++ show (Just (3 :: Int) <**> Just (+ 1)))
  putStrLn ("replace: " ++ show (7 <$ Just 'a', 'z' <$ [1, 2, 3 :: Int]))
