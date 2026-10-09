-- Haskell 2010 Report 11 (and 4.3.3): REJECT. Functor is not one of the derivable classes of Haskell 2010
-- (deriving it is a GHC extension, DeriveFunctor, which -XHaskell2010 does not enable).
module Main (main) where

data Box a = Box a deriving (Functor)

main :: IO ()
main = return ()
