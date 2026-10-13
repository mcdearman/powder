{-# LANGUAGE DataKinds, KindSignatures #-}
-- Nor a constructor of another data type
data Nat = Z | S Nat

data Tagged (n :: Nat) = Tagged

wrong :: Tagged 'True
wrong = Tagged

main :: IO ()
main = return ()
