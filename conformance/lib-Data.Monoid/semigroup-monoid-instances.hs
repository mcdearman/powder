-- Data.Monoid and Data.Semigroup: the classes, their instances for the Prelude's types, and the wrappers
import Data.Foldable (foldMap)
import Data.Monoid
import Data.Semigroup (Max (..), Min (..), Semigroup (..))

newtype MaxLen = MaxLen String deriving Show

instance Semigroup MaxLen where
  MaxLen a <> MaxLen b = if length b > length a then MaxLen b else MaxLen a

instance Monoid MaxLen where
  mempty = MaxLen ""

main :: IO ()
main = do
  putStrLn ("list: " ++ show ([1, 2, 3] <> [4 :: Int], mempty :: String, mconcat ["ab", "cd", "ef"]))
  putStrLn ("mappend: " ++ show (mappend "ab" "cd"))
  putStrLn ("ordering: " ++ show (LT <> GT, EQ <> GT, GT <> LT, mempty :: Ordering))
  putStrLn ("ordering-compare: " ++ show (compare 1 (1 :: Int) <> compare 'b' 'a'))
  putStrLn ("unit: " ++ show (() <> (), mempty :: ()))
  putStrLn ("maybe: " ++ show (Just "a" <> Nothing <> Just "b", (Nothing :: Maybe String) <> Nothing, mempty :: Maybe String))
  putStrLn ("pair: " ++ show (("a", [1 :: Int]) <> ("b", [2]), mempty :: (String, [Int])))
  putStrLn ("triple: " ++ show (("a", "b", "c") <> ("x", "y", "z")))
  putStrLn ("function: " ++ show ((show <> const "-" <> show) (5 :: Int)))
  putStrLn ("function-compare: " ++ show (((\a b -> compare (fst a) (fst b)) <> (\a b -> compare (snd a) (snd b))) (1 :: Int, 'b') (1, 'a')))
  putStrLn ("either: " ++ show (Left 1 <> Right 'a' <> Right 'b' :: Either Int Char, Left 1 <> Left 2 :: Either Int Char))
  putStrLn ("sum: " ++ show (Sum 3 <> Sum (4 :: Int), getSum (foldMap Sum [1 .. 10 :: Int]), mempty :: Sum Int))
  putStrLn ("product: " ++ show (Product 3 <> Product (4 :: Int), getProduct (foldMap Product [1 .. 5 :: Int])))
  putStrLn ("all-any: " ++ show (getAll (foldMap (All . even) [2, 4, 6 :: Int]), getAny (foldMap (Any . odd) [2, 4, 6 :: Int]), All True <> All False))
  putStrLn ("first-last: " ++ show (getFirst (First (Just 1) <> First Nothing <> First (Just (3 :: Int))), getLast (Last (Just 1) <> Last Nothing <> Last (Just (3 :: Int)))))
  putStrLn ("min-max: " ++ show (Min 3 <> Min (1 :: Int), Max 3 <> Max (1 :: Int), mempty :: Max Bool))
  putStrLn ("dual: " ++ show (getDual (Dual "ab" <> Dual "cd")))
  putStrLn ("endo: " ++ show (appEndo (Endo (+ 1) <> Endo (* 2)) (5 :: Int), appEndo (foldMap (Endo . (:)) "abc") ""))
  putStrLn ("user: " ++ show (mconcat (map MaxLen ["ab", "abcd", "abc", "wxyz"])))
  io <- (putStr "io: " >> return "joined") <> return "!"
  putStrLn io
