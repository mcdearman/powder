-- Haskell 2010 Report 4.4.1: type signatures naming several variables, and signatures for operators
module Main (main) where

inc, dec, twice :: Int -> Int
inc x = x + 1
dec x = x - 1
twice x = x * 2

-- one signature, each variable is separately polymorphic
first, second :: a -> a -> a
first x _ = x
second _ y = y

(|>), (<|) :: [a] -> a -> [a]
xs |> x = xs ++ [x]
xs <| x = x : xs

(<=>) :: Ord a => a -> a -> Ordering
(<=>) = compare

-- operators and identifiers mixed, with a context shared by all
(.+.), plus3, (.*.) :: Num a => a -> a -> a
a .+. b = a + b
plus3 a b = a + b + 3
a .*. b = a * b

-- signatures may come after the bindings and away from them
late x = not x

zero, one :: Int
(zero, one) = (0, 1)

late :: Bool -> Bool

main :: IO ()
main = do
  print ("inc dec twice", inc 1, dec 1, twice 21)
  print ("first second int", first (1 :: Int) 2, second (1 :: Int) 2)
  print ("first second other types", first 'a' 'b', second "x" "y")
  print ("snoc", [1, 2] |> (3 :: Int))
  print ("cons", "bc" <| 'a')
  print ("both", ([1] |> 2) <| (0 :: Int))
  print ("compare operator", 1 <=> (2 :: Int), 'b' <=> 'a', "x" <=> "x")
  print ("shared context int", (2 :: Int) .+. 3, plus3 (2 :: Int) 3, (2 :: Int) .*. 3)
  print ("shared context double", (0.5 :: Double) .+. 0.25, plus3 (0.5 :: Double) 0.5)
  print ("late signature", late True)
  print ("pattern binding", zero, one)
