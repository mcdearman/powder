-- Data.Functor: (<$>), (<$), ($>), (<&>) and void, and the instances of Functor for Either, functions and pairs
import Data.Functor

main :: IO ()
main = do
  putStrLn ("fmap-infix: " ++ show ((+ 1) <$> Just (1 :: Int), show <$> [1, 2 :: Int]))
  putStrLn ("replace-left: " ++ show ('x' <$ Just True, 0 <$ "abc"))
  putStrLn ("replace-right: " ++ show (Just True $> 'x', "abc" $> (0 :: Int)))
  putStrLn ("flipped: " ++ show (Just (3 :: Int) <&> (+ 1), [1, 2 :: Int] <&> even))
  putStrLn ("void: " ++ show (void (Just 'a'), void "abc"))
  putStrLn ("either: " ++ show (fmap (+ 1) (Right 3 :: Either String Int), fmap (+ 1) (Left "no" :: Either String Int)))
  putStrLn ("function: " ++ show (fmap (+ 1) (* 2) (5 :: Int)))
  putStrLn ("pair: " ++ show ((+ 1) <$> ("a", 3 :: Int)))
  r <- length <$> return "four"
  putStrLn ("io: " ++ show r)
