-- Haskell 2010 Report 4.3.1: class methods whose types constrain type variables other than the class variable
module Main (main) where

class Coll f where
  cmember :: Eq a => a -> f a -> Bool
  cshow :: Show a => f a -> String
  ctotal :: Num a => f a -> a
  cbest :: (Ord a, Show a) => f a -> String

instance Coll [] where
  cmember x xs = any (== x) xs
  cshow xs = concatMap show xs
  ctotal = foldr (+) 0
  cbest [] = "none"
  cbest xs = show (maximum xs)

instance Coll Maybe where
  cmember _ Nothing = False
  cmember x (Just y) = x == y
  cshow Nothing = "-"
  cshow (Just x) = show x
  ctotal Nothing = 0
  ctotal (Just x) = x
  cbest m = cshow m

class Conv a where
  convert :: Integral b => a -> b
  scaled :: (Num b, Show b) => b -> a -> String
  scaled k x = show (k * fromInteger (convert x))

instance Conv Bool where
  convert False = 0
  convert True = 1

instance Conv Char where
  convert c = fromIntegral (fromEnum c)
  scaled k c = c : show k

report :: (Coll f, Show a, Num a, Eq a) => f a -> (String, a, Bool)
report c = (cshow c, ctotal c, cmember 0 c)

main :: IO ()
main = do
  print ("cmember list", cmember 'a' "abc", cmember 'z' "abc")
  print ("cmember maybe", cmember (1 :: Int) (Just 1), cmember True Nothing)
  print ("cshow", cshow [1, 2, 3 :: Int], cshow (Just True), cshow (Nothing :: Maybe Int))
  print ("ctotal int", ctotal [1, 2, 3 :: Int], ctotal (Just (5 :: Int)))
  print ("ctotal double", ctotal [0.5, 0.25 :: Double], ctotal (Nothing :: Maybe Double))
  print ("cbest", cbest "hello", cbest ([] :: [Int]), cbest (Just (2 :: Int)))
  print ("report list", report [1, 0, 2 :: Int])
  print ("report maybe", report (Just (1.5 :: Double)))
  print ("convert", convert True :: Int, convert 'A' :: Integer)
  print ("scaled default", scaled (2 :: Int) True, scaled (1.5 :: Double) True)
  print ("scaled overridden", scaled (7 :: Int) 'x')
