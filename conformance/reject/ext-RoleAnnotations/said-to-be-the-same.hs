{-# LANGUAGE RoleAnnotations #-}
-- A parameter said to be the same type is not taken as another, though the two are kept alike
import Data.Coerce (coerce)

newtype Age = Age Int

type role Sorted nominal

data Sorted a = Sorted [a]

wrong :: Sorted Age -> Sorted Int
wrong = coerce

main :: IO ()
main = return ()
