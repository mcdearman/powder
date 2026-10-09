-- Haskell 2010 Report 11.4: derived Show for constructors declared with record syntax: `C {f1 = 1, f2 = "x"}`,
-- fields in declaration order, each field value shown at precedence 0, the whole parenthesised when d >= 11
module Main (main) where

data C = C {f1 :: Int, f2 :: String} deriving (Show)

data Single = Single {only :: Int} deriving (Show)

-- record and non-record constructors in one type
data Shape = Circle {radius :: Double} | Rect {width :: Double, height :: Double} | Point | Poly [Int] deriving (Show)

data Outer = Outer {inner :: C, flag :: Bool, opt :: Maybe C} deriving (Show)

data Poly2 a b = Poly2 {left :: a, right :: b} deriving (Show)

-- field names with primes, digits and underscores
data Odd = Odd {x' :: Int, y_1 :: Int, zLong_name2 :: Char} deriving (Show)

table :: Show a => String -> a -> IO ()
table label x = mapM_ (\d -> putStrLn (label ++ " @" ++ show d ++ ": " ++ showsPrec d x "")) [0 .. 11 :: Int]

main :: IO ()
main = do
  print (C 1 "x")
  print (C {f2 = "second", f1 = 1})
  print (C (-1) "")
  print (C {f1 = minBound, f2 = "quote\"d"})
  table "C" (C 1 "x")
  table "Single" (Single (-5))
  print (Just (C 1 "x"))
  print [C 1 "a", C 2 "b"]
  print (C 1 "x", C 2 "y")
  print (Left (C 0 "l") :: Either C Int)
  print (Single 1, Single (-1), Just (Single (-1)))
  print (Circle 1.5)
  print (Circle (-1.5))
  print (Rect 2 3)
  print (Rect {height = -1, width = -2})
  print Point
  print (Poly [1, -2])
  print [Circle 1, Rect 1 2, Point, Poly []]
  print (Just (Circle 1), Just Point, Just (Poly [1]))
  print (Outer (C 1 "x") True Nothing)
  print (Outer {opt = Just (C (-2) "y"), inner = C (-1) "x", flag = False})
  print (Just (Outer (C 1 "x") True (Just (C 2 "y"))))
  print (Poly2 1 'c' :: Poly2 Int Char)
  print (Poly2 (-1) (-2.5) :: Poly2 Int Double)
  print (Poly2 (Just (-1)) [Just (-2)] :: Poly2 (Maybe Int) [Maybe Int])
  print (Poly2 (Poly2 'a' 'b') (C 1 "x"))
  print (Poly2 (1 :: Int, -2 :: Int) (Left (-3) :: Either Int Int))
  print (Odd 1 2 'c')
  print (Odd {zLong_name2 = 'z', y_1 = -2, x' = -1})
  print ((C 1 "x") {f1 = 2})
  print (f1 (C 7 "s"), f2 (C 7 "s"), radius (Circle 2.5))
