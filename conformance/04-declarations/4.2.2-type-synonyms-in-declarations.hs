-- Haskell 2010 Report 4.2.2: type synonyms used in data, newtype, class and instance declarations and in contexts
module Main (main) where

type Point = (Int, Int)

type Path = [Point]

type Transform a = a -> a

type Shower a = a -> String

data Shape = Poly Path | Seg Point Point deriving (Eq, Show)

newtype Step = Step (Transform Int)

class Movable a where
  move :: Point -> Transform a
  origin :: a -> Point

instance Movable Shape where
  move d (Poly ps) = Poly (map (addPoint d) ps)
  move d (Seg p q) = Seg (addPoint d p) (addPoint d q)
  origin (Poly []) = (0, 0)
  origin (Poly (p : _)) = p
  origin (Seg p _) = p

addPoint :: Point -> Transform Point
addPoint (dx, dy) (x, y) = (x + dx, y + dy)

runStep :: Step -> Int -> Int
runStep (Step f) = f

-- a synonym under a context
showPair :: Show a => Shower (a, a)
showPair (x, y) = show x ++ "~" ++ show y

moveTwice :: Movable a => Point -> Transform a
moveTwice d = move d . move d

main :: IO ()
main = do
  print ("addPoint", addPoint (1, 2) (10, 20))
  print ("move poly", move (1, 1) (Poly [(0, 0), (2, 3)]))
  print ("move seg", move (0, 5) (Seg (1, 1) (2, 2)))
  print ("origin", origin (Poly []), origin (Seg (4, 5) (6, 7)))
  print ("moveTwice", moveTwice (1, 0) (Seg (0, 0) (1, 1)))
  print ("step", runStep (Step (+ 1)) 41)
  print ("showPair", showPair (True, False), showPair (1 :: Int, 2))
  let local :: Transform Path
      local = map (addPoint (100, 100))
  print ("local signature", local [(1, 2)])
