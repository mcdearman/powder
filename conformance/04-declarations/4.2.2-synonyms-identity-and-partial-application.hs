-- Haskell 2010 Report 4.2.2: a synonym with a parameter that is its whole right side, and a synonym for a
-- type constructor applied to some of its arguments
module Main (main) where

type Synonym = String

type Identity a = a

type Partial = Either Int

type Pair a = (a, a)

type Table k = [(k, Identity Synonym)]

str :: Identity Synonym
str = "abc"

right :: [Partial String]
right = [Left 1, Right "abc"]

swap :: Pair a -> Pair a
swap (x, y) = (y, x)

table :: Table Int
table = [(1, "one"), (2, str)]

count :: Partial a -> Identity Int
count = either id (const 0)

main :: IO ()
main = do
  print (str, length str, str == ("abc" :: [Char]))
  print (right, map count right)
  print (swap (1 :: Int, 2), swap (str, "d"))
  print (lookup 2 table, either show (map succ) (Right "abc" :: Partial Synonym))
