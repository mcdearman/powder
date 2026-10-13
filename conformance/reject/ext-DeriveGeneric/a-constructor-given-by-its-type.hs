{-# LANGUAGE DeriveGeneric, GADTs #-}
-- No instance of Generic is derived for a type whose constructors make it at types of their own choosing
import GHC.Generics (Generic)

data Term a where
  Number :: Int -> Term Int
  Truth :: Bool -> Term Bool
  deriving (Generic)

main :: IO ()
main = return ()
