{-# LANGUAGE KindSignatures #-}
-- DataKinds is not the Report's: without the extension a data type is no kind
data Nat = Z | S Nat

data Tagged (n :: Nat) = Tagged

main :: IO ()
main = return ()
