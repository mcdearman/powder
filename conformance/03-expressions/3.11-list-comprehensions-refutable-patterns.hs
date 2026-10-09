-- Haskell 2010 Report 3.11: a generator pattern that fails to match simply skips that element
module Main (main) where

data T = A Int | B Char | C Int Int
  deriving (Eq, Show)

ts :: [T]
ts = [A 1, B 'x', C 2 3, A 4, C 5 6]

main :: IO ()
main = do
  print ("Just", [x | Just x <- [Just 1, Nothing, Just 3]])
  print ("Nothing", length [() | Nothing <- [Just 1, Nothing, Just 3]])
  print ("cons", [x | (x : _) <- ["ab", "", "c"]])
  print ("literal in tuple", [y | (1, y) <- [(1, 'a'), (2, 'b'), (1, 'c')]])
  print ("Left/Right", [x | Left x <- [Left 1, Right 'a', Left 2]], [c | Right c <- [Left 1, Right 'a', Left 2]])
  print ("singleton list", [a | [a] <- [[1], [2, 3], [], [4]]])
  print ("two-element list", [a + b | [a, b] <- [[1], [2, 3], [], [4, 5]]])
  print ("string literal", length [() | "yes" <- ["yes", "no", "yes"]])
  print ("char literal", length [() | 'a' <- "banana"])
  print ("number literal", length [() | 0 <- [0, 1, 0, 2]])
  print ("nested", [x | Just (x, 'k') <- [Just (1, 'k'), Just (2, 'j'), Nothing, Just (3, 'k')]])
  print ("user constructors", [n | A n <- ts], [c | B c <- ts], [a + b | C a b <- ts])
  print ("as pattern", [(whole, h) | whole@(h : _) <- ["ab", "", "cd"]])
  print ("wildcard never fails", length [() | _ <- ts])
  print ("irrefutable never fails", length [() | ~(A _) <- ts])
  print ("variable never fails", length [t | t <- ts])
  print ("then guard", [x | Just x <- [Just 1, Nothing, Just 3, Just 4], odd x])
  print ("two refutable", [(x, y) | Just x <- [Just 1, Nothing], Right y <- [Left 'a', Right 'b']])
  print ("bool pattern", [n | (n, True) <- zip [1 ..] [True, False, True]])
  print ("all fail", [x | Just x <- [Nothing, Nothing]] :: [Int])
