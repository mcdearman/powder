{-# LANGUAGE DefaultSignatures #-}
-- A default is given a type only for a method of the class
class Describe a where
  describe :: a -> String
  default other :: Show a => a -> String
  describe _ = ""

main :: IO ()
main = return ()
