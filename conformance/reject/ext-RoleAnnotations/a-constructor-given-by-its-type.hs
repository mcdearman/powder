{-# LANGUAGE GADTs #-}
-- Nor is the parameter of a type whose constructors make it at types of their own choosing
import Data.Coerce (coerce)

newtype Age = Age Int

data Term a where
  Number :: Int -> Term Int
  Years :: Age -> Term Age

wrong :: Term Age -> Term Int
wrong = coerce

main :: IO ()
main = return ()
