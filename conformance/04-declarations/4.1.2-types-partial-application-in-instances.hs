-- Haskell 2010 Report 4.1.2: partially applied type constructors ([], Maybe, (,) e, (->) r, Either e, IO) as types of kind * -> *
module Main (main) where

class Named f where
  nameOf :: f a -> String

instance Named [] where
  nameOf _ = "list"

instance Named Maybe where
  nameOf _ = "maybe"

instance Named ((,) e) where
  nameOf _ = "pair"

instance Named ((->) r) where
  nameOf _ = "function"

instance Named (Either e) where
  nameOf _ = "either"

instance Named IO where
  nameOf _ = "io"

data Three a b c = Three a b c

instance Named (Three a b) where
  nameOf _ = "three"

both :: (Named f, Named g) => f a -> g b -> String
both x y = nameOf x ++ "+" ++ nameOf y

main :: IO ()
main = do
  print ("list", nameOf [1 :: Int])
  print ("string", nameOf "abc")
  print ("maybe", nameOf (Just 'x'))
  print ("pair", nameOf (1 :: Int, 'c'))
  print ("function", nameOf not)
  print ("function 2 args", nameOf (&&))
  print ("either", nameOf (Left 3 :: Either Int Char))
  print ("io", nameOf (return () :: IO ()))
  print ("three", nameOf (Three 'a' True (0 :: Int)))
  print ("both", both "s" (Just True))
  print ("both fn pair", both not ('a', 'b'))
