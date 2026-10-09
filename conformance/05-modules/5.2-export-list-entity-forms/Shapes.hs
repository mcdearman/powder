module Shapes
  ( Shape(..)
  , Dir(North, South), opposite, isHiddenDir
  , Opaque, mkOpaque, describeOpaque
  , Point(px), mkPoint, py
  , Coord, origin
  , HasArea(..)
  , Named(name)
  , Sized, sizeOf
  ) where

data Shape = Circle Int | Square Int deriving Show

data Dir = North | South | Hidden deriving (Show, Eq)

opposite :: Dir -> Dir
opposite North = Hidden
opposite South = North
opposite Hidden = South

isHiddenDir :: Dir -> Bool
isHiddenDir d = d == Hidden

data Opaque = MkOpaque Int

mkOpaque :: Int -> Opaque
mkOpaque = MkOpaque

describeOpaque :: Opaque -> String
describeOpaque (MkOpaque n) = "opaque " ++ show n

data Point = MkPoint { px :: Int, pyHidden :: Int }

mkPoint :: Int -> Int -> Point
mkPoint = MkPoint

py :: Point -> Int
py = pyHidden

type Coord = (Int, Int)

origin :: Coord
origin = (0, 0)

class HasArea a where
  area :: a -> Int
  perimeter :: a -> Int

instance HasArea Shape where
  area (Circle r) = 3 * r * r
  area (Square s) = s * s
  perimeter (Circle r) = 6 * r
  perimeter (Square s) = 4 * s

class Named a where
  name :: a -> String
  hidden :: a -> Int

instance Named Shape where
  name (Circle _) = "circle"
  name (Square _) = "square"
  hidden _ = 1

class Sized a where
  sides :: a -> Int

instance Sized Shape where
  sides (Circle _) = 0
  sides (Square _) = 4

sizeOf :: Sized a => a -> Int
sizeOf = sides
