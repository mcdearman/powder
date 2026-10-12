module Data.Either
  ( Either (..), either, lefts, rights, partitionEithers, isLeft, isRight, fromLeft, fromRight
  )
where

lefts :: [Either a b] -> [a]
lefts xs = [a | Left a <- xs]

rights :: [Either a b] -> [b]
rights xs = [b | Right b <- xs]

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr (either left right) ([], [])
  where
    left a ~(ls, rs) = (a : ls, rs)
    right b ~(ls, rs) = (ls, b : rs)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

isRight :: Either a b -> Bool
isRight (Left _) = False
isRight (Right _) = True

fromLeft :: a -> Either a b -> a
fromLeft _ (Left a) = a
fromLeft a _ = a

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _ = b
