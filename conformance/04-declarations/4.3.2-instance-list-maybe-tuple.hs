-- Haskell 2010 Report 4.3.2: instances for [a], Maybe a, tuples, unit and Either, with contexts
module Main (main) where

class Size a where
  size :: a -> Int

instance Size Int where
  size _ = 1

instance Size Bool where
  size _ = 1

instance Size Char where
  size _ = 1

instance Size () where
  size () = 0

instance Size a => Size [a] where
  size xs = sum (map size xs)

instance Size a => Size (Maybe a) where
  size Nothing = 0
  size (Just x) = 1 + size x

instance (Size a, Size b) => Size (a, b) where
  size (x, y) = size x + size y

instance (Size a, Size b, Size c) => Size (a, b, c) where
  size (x, y, z) = size x + size y + size z

instance (Size a, Size b) => Size (Either a b) where
  size (Left x) = size x
  size (Right y) = 10 * size y

-- prefix forms of the same instance heads, for another class
class Shape a where
  shape :: a -> String

instance Shape Bool where
  shape _ = "b"

instance Shape a => Shape ([] a) where
  shape [] = "[]"
  shape (x : _) = "[" ++ shape x ++ "]"

instance (Shape a, Shape b) => Shape ((,) a b) where
  shape (x, y) = "(" ++ shape x ++ "," ++ shape y ++ ")"

main :: IO ()
main = do
  print ("int", size (5 :: Int))
  print ("unit", size ())
  print ("string", size "hello")
  print ("list of lists", size [[1, 2], [3 :: Int], []])
  print ("maybe", size (Just 'x'), size (Nothing :: Maybe Int))
  print ("maybe list", size (Just "abc"))
  print ("pair", size ('a', True))
  print ("triple", size (1 :: Int, "ab", Just ()))
  print ("either", size (Left 'a' :: Either Char Bool), size (Right True :: Either Char Bool))
  print ("nested", size [(Just True, "xy"), (Nothing, "")])
  print ("list of units", size [(), (), ()])
  print ("shape", shape [True], shape ([] :: [Bool]), shape (True, [False]))
  print ("shape nested", shape [[(True, True)]])
