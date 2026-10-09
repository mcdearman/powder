-- Haskell 2010 Report 3.15.1: field labels are selector functions
module Main (main) where

data P = P { px :: Int, py :: Int, pname :: String }
  deriving (Eq, Show)

data Shape
  = Circle { label :: String, radius :: Int }
  | Rect { label :: String, width, height :: Int }
  | Dot { label :: String }
  deriving (Eq, Show)

data Box a = Box { content :: a, count :: Int }
  deriving (Eq, Show)

newtype Wrap = Wrap { unwrap :: [Int] }
  deriving (Eq, Show)

data Op = Op { apply :: Int -> Int, opName :: String }

shapes :: [Shape]
shapes = [Circle "c" 1, Rect "r" 2 3, Dot "d"]

main :: IO ()
main = do
  let p = P 1 2 "pt"
  print ("select", px p, py p, pname p)
  print ("positional construction", px (P 5 6 "x"), py (P 5 6 "x"))
  print ("selector as function", map px [P 1 2 "a", P 3 4 "b"])
  print ("composition", (succ . py) p, (length . pname) p)
  print ("shared field in all constructors", map label shapes)
  print ("field in some constructors", radius (head shapes), width (shapes !! 1), height (shapes !! 1))
  print ("polymorphic field", content (Box 'c' 1), content (Box [True] 2), count (Box () 3))
  print ("nested", content (content (Box (Box "in" 1) 2)))
  print ("newtype field", unwrap (Wrap [1, 2]), (sum . unwrap) (Wrap [1, 2, 3]))
  print ("function field", apply (Op (+ 1) "inc") 5, opName (Op (+ 1) "inc"))
  print ("function field applied in map", map (apply (Op (* 2) "dbl")) [1, 2, 3])
  print ("lazy unselected field", px (P 1 undefined undefined))
  print ("selector in section", map (($ p) . (show .)) [px, py])
  print ("sort of fields", zip (map px [P 1 2 "a", P 3 4 "b"]) (map pname [P 1 2 "a", P 3 4 "b"]))
