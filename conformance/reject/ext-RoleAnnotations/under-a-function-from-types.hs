{-# LANGUAGE TypeFamilies #-}
-- A parameter a function from types is applied to decides what the type is, and must be the same
import Data.Coerce (coerce)

newtype Age = Age Int

type family Kept a where
  Kept Int = Bool
  Kept Age = Char

data Holder a = Holder (Kept a)

wrong :: Holder Age -> Holder Int
wrong = coerce

main :: IO ()
main = return ()
