-- Haskell 2010 Report 4.3.1, 4.3.2: several methods in one signature, defaults that call each other, and
-- instances with an empty body or one overriding a single method
module Main (main) where

class Described a where
  short, long :: a -> String
  short _ = "thing"
  long x = "a " ++ short x

instance Described ()

instance Described Bool where
  short True = "yes"
  short False = "no"

instance Described Char where
  long c = "the character " ++ [c]

instance Described a => Described [a] where
  short xs = unwords (map short xs)

main :: IO ()
main = do
  putStrLn (long ())
  putStrLn (short ())
  putStrLn (long True)
  putStrLn (short 'x')
  putStrLn (long 'x')
  putStrLn (long [True, False])
  putStrLn (long [(), ()])
