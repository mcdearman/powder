-- Haskell 2010 Report 3.15.3: updates using field labels (non-destructive; several fields; shared fields; type-changing)
module Main (main) where

data P = P { px :: Int, py :: Int, pname :: String }
  deriving (Eq, Show)

data Shape
  = Circle { label :: String, radius :: Int }
  | Rect { label :: String, width, height :: Int }
  deriving (Eq, Show)

data Box a = Box { content :: a, tag :: String }
  deriving (Eq, Show)

data Two a = Two { one :: a, two :: a }
  deriving (Eq, Show)

rename :: String -> Shape -> Shape
rename s shape = shape { label = s }

main :: IO ()
main = do
  let p = P 1 2 "p"
      c = Circle "c" 5
      r = Rect "r" 2 3
  print ("single", p { px = 10 })
  print ("original unchanged", p)
  print ("multiple", p { py = 20, pname = "q" })
  print ("all, any order", p { pname = "z", px = 7, py = 8 })
  print ("chained", (p { px = 5 }) { py = 6 })
  print ("uses old value", p { px = px p + 100 })
  print ("shared field, both constructors", map (rename "new") [c, r])
  print ("field of one constructor", c { radius = 9 }, r { width = 7, height = 8 })
  print ("shared and specific", r { label = "R", height = 0 })
  -- update binds tighter than function application
  print ("binds tighter than application", px p { px = 42 })
  print ("on parenthesised expression", (P 0 0 "") { pname = "set" })
  print ("on function result", (rename "x" c) { radius = 0 })
  print ("type-changing", (Box 1 "n") { content = 'c' }, (Box 1 "n") { content = [True] })
  print ("type-changing both fields", (Two 1 2) { one = 'a', two = 'b' })
  print ("same type one field", (Two 1 2) { two = 5 })
  print ("update with bottom unforced", px (p { py = undefined }))
  print ("update of missing-field record", pname ((P { pname = "partial" }) { px = 3 }))
  print ("nested records", Box (p { px = 0 }) "outer")
  print ("translation", p { px = 3 } == (case p of { P _ b n -> P 3 b n }))
  print ("in lambda", map (\q -> q { py = 0 }) [p, p { px = 2 }])
