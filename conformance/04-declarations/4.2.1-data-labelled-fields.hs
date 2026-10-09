-- Haskell 2010 Report 4.2.1: constructors with labelled fields; declaration forms and positional use
module Main (main) where

data Person = Person { name :: String, age :: Int }

data Point = Point { px, py :: Int } deriving (Eq, Show)

data Config = Config { verbose :: Bool, level :: !Int, tags :: [String] }

data Boxed a = Boxed { unBoxed :: a, note :: String }

-- a record constructor with no fields, and one mixed with a positional constructor
data Odd = NoFields {} | Positional Int Int

greet :: Person -> String
greet Person { name = n } = "hello " ++ n

older :: Person -> Person
older p = p { age = age p + 1 }

isRecord :: Odd -> Bool
isRecord NoFields {} = True
isRecord Positional {} = False

summary :: Config -> String
summary (Config v l ts) = show v ++ "/" ++ show l ++ "/" ++ concat ts

main :: IO ()
main = do
  let alice = Person { name = "alice", age = 30 }
      bob = Person "bob" 25
      swapped = Person { age = 41, name = "carol" }
  print ("select", name alice, age alice)
  print ("positional construction", name bob, age bob)
  print ("labels in any order", name swapped, age swapped)
  print ("pattern", greet bob)
  print ("update", age (older alice), name (older alice))
  print ("selectors are functions", map age [alice, bob, swapped])
  print ("several labels one type", Point { px = 1, py = 2 })
  print ("point positional", Point 3 4 == Point { py = 4, px = 3 })
  print ("point update", (Point 1 2) { py = 9 })
  let cfg = Config { verbose = True, level = 3, tags = ["a", "b"] }
  print ("config", summary cfg)
  print ("config update two fields", summary (cfg { verbose = False, tags = [] }))
  print ("strict field", level cfg)
  print ("parameterised", unBoxed (Boxed 'x' "n"), note (Boxed True "flag"))
  print ("update changes type parameter", unBoxed ((Boxed 'x' "n") { unBoxed = True }))
  print ("empty braces", isRecord NoFields, isRecord (NoFields {}), isRecord (Positional 1 2))
