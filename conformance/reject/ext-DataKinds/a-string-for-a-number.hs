{-# LANGUAGE DataKinds, KindSignatures #-}
-- A string as a type is not of the kind a number as a type is
import GHC.TypeLits (Nat)

newtype Mod (n :: Nat) = Mod Integer

wrong :: Mod "seven"
wrong = Mod 0

main :: IO ()
main = return ()
