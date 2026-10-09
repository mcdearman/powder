module Stack (Stack, empty, push, pop, top, isEmpty, toList) where

data Stack a = Empty | Push a (Stack a) deriving (Eq, Show)

instance Functor Stack where
  fmap _ Empty = Empty
  fmap f (Push x s) = Push (f x) (fmap f s)

empty :: Stack a
empty = Empty

push :: a -> Stack a -> Stack a
push = Push

pop :: Stack a -> Maybe (Stack a)
pop Empty = Nothing
pop (Push _ s) = Just s

top :: Stack a -> Maybe a
top Empty = Nothing
top (Push x _) = Just x

isEmpty :: Stack a -> Bool
isEmpty Empty = True
isEmpty _ = False

toList :: Stack a -> [a]
toList Empty = []
toList (Push x s) = x : toList s
