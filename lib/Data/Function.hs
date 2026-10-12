module Data.Function
  ( id, const, (.), flip, ($), (&), on, fix
  )
where

infixl 0 `on`
infixl 1 &

(&) :: a -> (a -> b) -> b
x & f = f x

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on op f x y = f x `op` f y

fix :: (a -> a) -> a
fix f = let x = f x in x
