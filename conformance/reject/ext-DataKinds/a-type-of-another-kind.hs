{-# LANGUAGE DataKinds, KindSignatures #-}
-- A parameter said to be of a data type's kind is not given a type of another
data Nat = Z | S Nat

data Tagged (n :: Nat) = Tagged

wrong :: Tagged Int
wrong = Tagged

main :: IO ()
main = return ()
