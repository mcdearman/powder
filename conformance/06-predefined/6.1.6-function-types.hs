-- Haskell 2010 Report 6.1.6: functions as values; id, const, (.), flip, ($), until, asTypeOf
twice :: (a -> a) -> a -> a
twice f = f . f

compose3 :: (c -> d) -> (b -> c) -> (a -> b) -> a -> d
compose3 f g h = f . g . h

applyAll :: [a -> a] -> a -> a
applyAll fs x = foldr ($) x fs

main :: IO ()
main = do
  print ("id", id (3 :: Int), id "s", map id [True, False], id id 'c')
  print ("const", const (1 :: Int) 'x', const 'a' (undefined :: Int), map (const '-') "abc")
  print ("compose", ((+ 1) . (* 2)) (5 :: Int), (show . not) True)
  print ("compose chain", (length . filter even . map (* 3)) [1 .. 10 :: Int])
  print ("compose is right assoc, prec 9", (negate . abs . subtract 10) (3 :: Int))
  print ("compose3", compose3 show (+ 1) length "abcd")
  print ("flip", flip (-) 1 (10 :: Int), flip (:) [2] (1 :: Int), flip const 'a' 'b')
  print ("flip twice", flip (flip (-)) 10 (1 :: Int))
  print ("apply", (+ 1) $ (2 :: Int), negate $ abs $ subtract 10 (3 :: Int))
  print ("apply section", map ($ 3) [(+ 1), (* 2), subtract (1 :: Int)])
  print ("apply has precedence 0", show $ 1 + 2 * (3 :: Int))
  print ("zipWith apply", zipWith ($) [succ, pred] "bb", zipWith id [not] [True])
  print ("until", until (> 100) (* 2) (1 :: Int), until even (+ 1) (4 :: Int))
  print ("until with compose", until ((> 3) . length) ('x' :) "")
  print ("asTypeOf", 3 `asTypeOf` (1.5 :: Double))
  print ("sections", (subtract 1) (10 :: Int), (`div` 2) (9 :: Int), (2 ^) (5 :: Int) :: Int)
  print ("twice", twice (+ 3) (10 :: Int), (twice . twice) (* 2) (1 :: Int), twice twice (++ "!") "hi")
  print ("applyAll", applyAll [(+ 1), (* 2), subtract 3] (10 :: Int))
  print ("functions in data", fst (succ, not) 'b', snd (not, pred) 'b', (fst (id, const)) 'q')
  print ("partial application", map (max 3) [1, 5 :: Int], let add = (+) :: Int -> Int -> Int in add 2 3)
  print ("lambda", (\x y -> x * 10 + y) 4 (2 :: Int), (\_ -> 'k') (undefined :: Bool))
