-- Haskell 2010 Report 4.3.2: the instance context must imply the constraints its methods need;
-- showing the element requires Show a, which the instance does not assume
module Main (main) where

class Pretty a where
  pretty :: a -> String

newtype Box a = Box a

instance Pretty (Box a) where
  pretty (Box x) = show x

main :: IO ()
main = putStrLn "unreachable"
