{-# LANGUAGE GHC2021 #-}
-- Data.List.NonEmpty, with sconcat and stimes; and Data.Either, Data.Ord, Data.Tuple, Data.Bool, Data.Bifunctor,
-- Data.Functor.Identity, Data.Functor.Const, Data.IORef, and what Data.List and Control.Monad have beyond the Report
import Control.Monad
import Data.Bifunctor
import Data.Bool (bool)
import Data.Either
import Data.Functor.Const
import Data.Functor.Identity
import Data.IORef
import Data.List
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty as N
import Data.Ord
import Data.Semigroup
import Data.Tuple (swap)

main :: IO ()
main = do
  let n = 3 :| [1, 2 :: Int]
  putStrLn ("nonempty-show: " ++ show (n, Just ('a' :| "bc")))
  putStrLn ("nonempty-parts: " ++ show (N.head n, N.tail n, N.last n, N.init n, N.length n, N.toList n))
  putStrLn ("nonempty-make: " ++ show (nonEmpty ([] :: [Int]), nonEmpty [1, 2 :: Int], N.fromList "ab", N.singleton 'x'))
  putStrLn ("nonempty-change: " ++ show (N.map (* 2) n, N.reverse n, N.sort n, N.cons 0 n, 9 N.<| n, N.appendList n [7]))
  putStrLn ("nonempty-zip: " ++ show (N.zip n ('a' :| "bcd"), N.zipWith (+) n n))
  putStrLn ("nonempty-lists: " ++ show (N.filter odd n, N.takeWhile (> 1) n, N.drop 1 n, N.take 2 n))
  putStrLn ("nonempty-group: " ++ show (N.group "aabccc", N.nub (1 :| [1, 2, 1 :: Int])))
  putStrLn ("nonempty-classes: " ++ show (fmap (+ 1) n, sum n, maximum n, length n, elem 2 n, foldr (:) [] n, n <> n))
  putStrLn ("nonempty-monad: " ++ show (n >>= \x -> x :| [x * 10], (,) <$> ('a' :| "b") <*> (True :| [False])))
  putStrLn ("nonempty-traverse: " ++ show (traverse (\x -> if x > 0 then Just x else Nothing) n, sequenceA (Just 1 :| [Nothing :: Maybe Int])))
  putStrLn ("nonempty-order: " ++ show (compare n (3 :| [1]), n == n, N.take 3 (N.iterate (* 2) (1 :: Int)), N.xor (True :| [True, True])))
  putStrLn ("sconcat: " ++ show (sconcat ("ab" :| ["cd", "ef"]), sconcat (Max 1 :| [Max 5, Max (3 :: Int)]), sconcat (Just [1 :: Int] :| [Nothing])))
  putStrLn ("stimes: " ++ show (stimes (3 :: Int) "ab", stimes (5 :: Integer) (Sum (2 :: Int)), stimes (1 :: Int) [()]))
  putStrLn ("first-last: " ++ show (First 1 <> First (2 :: Int), Last 1 <> Last (2 :: Int), getMin (Min 'a' <> Min 'b')))
  let es = [Left 'a', Right (1 :: Int), Left 'b', Right 2]
  putStrLn ("either: " ++ show (lefts es, rights es, partitionEithers es, isLeft (head es), isRight (head es)))
  putStrLn ("either-from: " ++ show (fromLeft 'z' (Left 'a' :: Either Char Int), fromLeft 'z' (Right 1 :: Either Char Int), fromRight 0 (Left 'a' :: Either Char Int)))
  putStrLn ("ord: " ++ show (comparing snd (1 :: Int, 'b') (2, 'a'), clamp (0, 10) (15 :: Int), clamp (0, 10) (-5 :: Int)))
  putStrLn ("down: " ++ show (sortBy (comparing Down) [3, 1, 2 :: Int], compare (Down 1) (Down (2 :: Int)), Down 'x', getDown (Down 'y')))
  putStrLn ("sortOn: " ++ show (sortOn negate [3, 1, 2 :: Int], sortOn snd [(1 :: Int, 'b'), (2, 'a')], sortOn Down "hello"))
  putStrLn ("tuple-bool: " ++ show (swap (1 :: Int, 'a'), bool "no" "yes" True, bool "no" "yes" False))
  putStrLn ("bifunctor: " ++ show (bimap show not (1 :: Int, True), first (+ 1) (1 :: Int, 'a'), second succ (1 :: Int, 'a')))
  putStrLn ("bifunctor-either: " ++ show (bimap length negate (Left "abc" :: Either String Int), first length (Right 5 :: Either String Int), second negate (Right 5 :: Either String Int)))
  putStrLn ("identity: " ++ show (runIdentity (Identity 5 >>= \x -> return (x + 1 :: Int)), fmap succ (Identity 'a'), Identity [1 :: Int] <> Identity [2]))
  putStrLn ("identity-traverse: " ++ show (runIdentity (traverse (Identity . (+ 1)) [1, 2, 3 :: Int]), sum (Identity (4 :: Int))))
  putStrLn ("const: " ++ show (getConst (traverse (\x -> Const [x]) (Just 'a')), getConst (Const "ab" <*> Const "cd"), fmap not (Const 'k'), length (Const 'k')))
  putStrLn ("list-more: " ++ show (uncons "abc", uncons "", unsnoc "abc", unsnoc "", singleton 'x', dropWhileEnd (== ' ') "ab  "))
  putStrLn ("list-scans: " ++ show (isSubsequenceOf "ace" "abcde", isSubsequenceOf "aec" "abcde", scanl' (+) 0 [1, 2, 3 :: Int], take 4 (iterate' (* 3) (1 :: Int))))
  putStrLn ("monad-more: " ++ show (mfilter even (Just (2 :: Int)), mfilter even (Just (3 :: Int)), mfilter odd [1, 2, 3 :: Int]))
  r <- newIORef (0 :: Int)
  forM_ [1 .. 10] (\x -> modifyIORef r (+ x))
  a <- readIORef r
  writeIORef r 100
  modifyIORef' r (* 2)
  b <- atomicModifyIORef r (\x -> (x + 1, x))
  c <- readIORef r
  r' <- newIORef (0 :: Int)
  putStrLn ("ioref: " ++ show (a, b, c, r == r, r == r'))
  s <- (+ 1) <$!> return (1 :: Int)
  when (s == 2) (putStrLn "strict-fmap: 2")
  putStrLn ("word: " ++ show (maxBound :: Word, (fromIntegral (3 :: Int) :: Word) + 4))
  print =<< traverse (\x -> x <$ when (x > 1) (putStrLn ("when-applicative: " ++ show x))) [1, 2, 3 :: Int]
