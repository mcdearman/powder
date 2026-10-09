-- Haskell 2010 Report 3.15.2: construction using field labels (any order; omitted lazy fields are undefined; C {} for any constructor)
module Main (main) where

data P = P { px :: Int, py :: Int, pname :: String }
  deriving (Eq, Show)

data Shape
  = Circle { radius :: Int }
  | Rect { width, height :: Int }
  deriving (Eq, Show)

data Plain = Plain Int Char | Empty
  deriving (Eq, Show)

isPlain :: Plain -> Bool
isPlain (Plain _ _) = True
isPlain Empty = False

main :: IO ()
main = do
  print ("declared order", P { px = 1, py = 2, pname = "p" })
  print ("other order", P { pname = "p", py = 2, px = 1 })
  print ("orders equal", P { px = 1, py = 2, pname = "p" } == P { py = 2, pname = "p", px = 1 })
  print ("same as positional", P { px = 1, py = 2, pname = "p" } == P 1 2 "p")
  print ("positional on record constructor", P 3 4 "q")
  print ("multi-constructor", Circle { radius = 5 }, Rect { height = 2, width = 3 })
  print ("field expressions", P { px = 1 + 1, py = length "abc", pname = "a" ++ "b" })
  -- construction binds tighter than function application
  print ("binds tighter than application", Just P { px = 1, py = 2, pname = "j" })
  print ("as argument", px P { px = 9, py = 0, pname = "" })
  -- omitted non-strict fields are bottom, which is harmless unless demanded
  print ("missing lazy fields", px (P { px = 7 }), pname (P { pname = "only name" }))
  print ("missing field in other constructor", width (Rect { width = 4 }))
  -- C {} is legal for any constructor, record or not
  print ("empty braces on record", case P {} of { P _ _ _ -> "P" })
  print ("empty braces on plain", isPlain (Plain {}), isPlain Empty {})
  print ("empty braces on nullary", Empty {}, Empty {} == Empty)
  print ("empty braces on Prelude constructors", case Just {} of { Just _ -> "Just"; Nothing -> "Nothing" }, Nothing {} == (Nothing :: Maybe Int))
  print ("no spaces", P{px=1,py=2,pname="n"})
  print ("trailing application", map (\n -> Circle { radius = n }) [1, 2])
