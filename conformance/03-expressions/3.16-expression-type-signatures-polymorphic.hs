-- Haskell 2010 Report 3.16: e :: t = let { v :: t; v = e } in v; polymorphic and constrained signatures on expressions
module Main (main) where

main :: IO ()
main = do
  print ("polymorphic id", (id :: a -> a) 3, (id :: a -> a) 'c')
  print ("less general", (id :: Int -> Int) 3, ((\x -> x) :: Char -> Char) 'c')
  print ("constrained", (negate :: Num a => a -> a) 5, ((+) :: Num n => n -> n -> n) 1 2)
  print ("two constraints", ((\x -> show (x + 1)) :: (Show a, Num a) => a -> String) 1)
  print ("polymorphic empty list", length ([] :: [a]), null ([] :: [b]))
  print ("polymorphic const", (const :: a -> b -> a) 'k' ())
  print ("polymorphic map", (map :: (a -> b) -> [a] -> [b]) succ "abc")
  print ("specialised function", map (fromIntegral :: Int -> Integer) [1, 2])
  print ("specialised show", (show :: Int -> String) 42, (show :: Bool -> String) True)
  print ("instantiate polymorphic value", ((minBound :: Bounded b => b) :: Char) == '\0')
  print ("composition", (toEnum . fromEnum :: Char -> Char) 'a')
  print ("section", ((+ 1) :: Int -> Int) 1, ((`div` 2) :: Integer -> Integer) 9)
  print ("monads", (return 1 :: Maybe Int), (return 1 :: [Int]))
  print ("higher-order", ((\f -> f . f) :: (a -> a) -> a -> a) (* 3) 2)
  -- the translation into a let with a declaration signature
  print ("translation", (3 + 4 :: Int) == (let { v :: Int; v = 3 + 4 } in v))
  print ("translation polymorphic", let { v :: a -> [a]; v = \x -> [x] } in (v 1, v 'c'))
  print ("signature then use at two types", let f = ((\x -> (x, x)) :: a -> (a, a)) in (f True, f "s"))
  print ("type variables are local to each signature", ((\x -> x) :: a -> a) ((3 :: Int) + ((\y -> y) :: a -> a) 4))
  print ("type synonym", ("abc" :: String) == (['a', 'b', 'c'] :: [Char]))
  print ("function arrow nesting", ((\f x -> f x x) :: (a -> a -> b) -> a -> b) (+) 21)
