-- Records: fields as functions, building, updating and matching by field;
-- and constructors written between their fields.

data Point = Point {px, py :: Int} deriving (Eq, Show)

data Shape
  = Circle {centre :: Point, radius :: Int}
  | Box {centre :: Point, width :: Int, height :: !Int}

data Expr = Lit Int | Expr :+: Expr | Expr :*: Expr | Expr `Minus` Expr deriving (Show)

infixl 6 :+:
infixl 7 :*:

eval :: Expr -> Int
eval (Lit n) = n
eval (a :+: b) = eval a + eval b
eval (a :*: b) = eval a * eval b
eval (a `Minus` b) = eval a - eval b

area :: Shape -> Int
area Circle {radius = r} = 3 * r * r
area Box {width = w, height = h} = w * h

describe :: Shape -> String
describe s = show (px (centre s), py (centre s)) ++ " area " ++ show (area s)

main :: IO ()
main = do
  let origin = Point {px = 0, py = 0}
      p = origin {py = 5}
      shapes = [Circle {centre = p, radius = 2}, Box {height = 3, width = 4, centre = origin}]
      moved = map (\s -> s {centre = (centre s) {px = 9}}) shapes
  print (origin, p, p == origin {py = 5})
  putStrLn (unwords (map describe shapes))
  putStrLn (unwords (map describe moved))
  print (width (shapes !! 1), [radius s | s@Circle {} <- shapes])
  print (eval (Lit 2 :+: Lit 3 :*: Lit 4 `Minus` Lit 1))
  print (Lit 1 :+: Lit 2 :*: Lit 3, (Lit 1 :+: Lit 2) :*: Lit 3)
