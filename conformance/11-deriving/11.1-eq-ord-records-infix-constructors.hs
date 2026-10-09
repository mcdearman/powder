-- Haskell 2010 Report 11.1: derived Eq and Ord for record types (fields compared in declaration order) and for
-- infix constructors (operators and backquoted names); fixity declarations do not affect the comparison
module Main (main) where

data Person = Person {name :: String, age :: Int} deriving (Eq, Ord, Show)

-- same field in two constructors
data Item = Book {title :: String, pages :: Int} | Pen {title :: String} | Blank deriving (Eq, Ord, Show)

infixr 5 :+:

infixl 6 :*:

data Op = Int :+: Int | Int :*: Int deriving (Eq, Ord, Show)

data Rel = Int `Before` Int | Int `After` Int deriving (Eq, Ord, Show)

infix 5 :=>

data Arrow a b = a :=> b deriving (Eq, Ord, Show)

main :: IO ()
main = do
  print ("record Eq", Person "ann" 30 == Person "ann" 30, Person "ann" 30 == Person "ann" 31, Person "ann" 30 /= Person "bob" 30)
  print ("record Eq with field syntax", Person {age = 30, name = "ann"} == Person "ann" 30, Person {name = "ann", age = 30} == Person {age = 30, name = "ann"})
  print ("record Ord: declared field order", Person "ann" 99 < Person "bob" 1, compare (Person "ann" 30) (Person "ann" 31), compare (Person "bob" 1) (Person "ann" 99))
  print ("record Ord: construction order irrelevant", Person {age = 99, name = "ann"} < Person {age = 1, name = "bob"})
  print ("record max/min", max (Person "ann" 30) (Person "ann" 31), min (Person "zed" 1) (Person "amy" 99))
  print ("record update", (Person "ann" 30) {age = 31} > Person "ann" 30, (Person "ann" 30) {name = "al"} < Person "ann" 30)
  print ("multi-constructor record", Book "a" 1 < Pen "a", Pen "zzz" < Blank, Book "b" 1 > Book "a" 99, Book "a" 1 < Book "a" 2)
  print ("multi-constructor record Eq", Book "a" 1 == Book "a" 1, Pen "a" == Pen "a", Pen "a" == Book "a" 0, Blank == Blank)
  print ("shared field selector", title (Book "t" 1) == title (Pen "t"), compare (Pen "a") (Pen "b"))
  print ("infix Eq", 1 :+: 2 == 1 :+: 2, 1 :+: 2 == 2 :+: 1, 1 :+: 2 == 1 :*: 2, (1 :*: 2) /= (1 :*: 3))
  print ("infix Ord: constructor order", 9 :+: 9 < 0 :*: 0, compare (0 :*: 0) (9 :+: 9), max (1 :+: 1) (1 :*: 1))
  print ("infix Ord: fields", 1 :+: 2 < 1 :+: 3, 1 :+: 9 < 2 :+: 0, compare (3 :*: 4) (3 :*: 4), compare (3 :*: 5) (3 :*: 4))
  print ("infix used prefix", (:+:) 1 2 == 1 :+: 2, (:*:) 1 2 > (:+:) 1 2)
  print ("backquoted Eq", (1 `Before` 2) == (1 `Before` 2), (1 `Before` 2) == Before 1 2, (1 `Before` 2) == (1 `After` 2))
  print ("backquoted Ord", (9 `Before` 9) < (0 `After` 0), compare (1 `After` 2) (1 `After` 3), max (1 `Before` 5) (2 `Before` 0))
  print ("parameterised infix", 'a' :=> True == 'a' :=> True, 'a' :=> True < 'b' :=> False, compare (1 :=> "b") ((1 :: Int) :=> "a"))
  print ("nested infix", (1 :=> 'x') :=> "s" == (1 :=> 'x') :=> "s", ((1 :: Int) :=> 'x') :=> "s" < (1 :=> 'y') :=> "a")
  print ("in lists", [1 :+: 2, 3 :*: 4] == [1 :+: 2, 3 :*: 4], maximum [1 :+: 2, 3 :*: 4, 5 :+: 6], minimum [Pen "x", Blank, Book "y" 3])
