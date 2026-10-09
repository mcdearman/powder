-- Haskell 2010 Report 11.4: derived Show with fields of type String, Char, list, tuple, unit, Maybe, Either, Bool, Ordering:
-- each field is shown with showsPrec 11 (strings use the string syntax via showList)
module Main (main) where

data T = T () (Int, Char) String [Int] Char deriving (Show)

data S = S String Char [String] [Char] deriving (Show)

data U = U (Maybe Int) (Either Int String) Bool Ordering deriving (Show)

data V = V [Maybe Int] [(Int, Bool)] ((), ()) [[Int]] [()] deriving (Show)

data W = W (Int, Int, Int) ((Int, Int), [Int]) (Maybe (Int, Int)) deriving (Show)

data X = X (Maybe (Maybe Int)) (Maybe [Int]) (Either (Maybe Int) [Bool]) deriving (Show)

main :: IO ()
main = do
  print (T () (1, 'a') "s" [1, 2] 'c')
  print (T () (-1, '\n') "" [] '\'')
  print (T () (0, '"') "with \"quotes\" and \\ and \n newline" [-1] '\t')
  print (S "abc" 'd' ["e", "", "f g"] ['h', 'i'])
  print (S "" ' ' [] "")
  print (S "\1234\&5" '\200' ["\SO\&H"] "tab\there")
  print (U (Just 1) (Left 2) True LT)
  print (U (Just (-1)) (Right "r") False GT)
  print (U Nothing (Left (-2)) True EQ)
  print (V [Just 1, Nothing, Just (-1)] [(1, True), (-2, False)] ((), ()) [[1], [], [-2, 3]] [(), ()])
  print (V [] [] ((), ()) [] [])
  print (W (1, 2, 3) ((4, 5), [6]) (Just (7, 8)))
  print (W (-1, -2, -3) ((-4, -5), [-6]) (Just (-7, -8)))
  print (W (0, 0, 0) ((0, 0), []) Nothing)
  print (X (Just (Just 1)) (Just [1, 2]) (Left (Just 3)))
  print (X (Just Nothing) (Just []) (Right [True, False]))
  print (X (Just (Just (-1))) Nothing (Left Nothing))
  print (Just (T () (1, 'a') "s" [1, 2] 'c'))
  print [U Nothing (Left 0) False LT]
  print (S "a" 'b' [] "", U Nothing (Right "") True EQ)
  putStrLn (showsPrec 11 (T () (1, 'a') "s" [1, 2] 'c') "")
  putStrLn (showsPrec 11 (X Nothing Nothing (Right [])) "")
