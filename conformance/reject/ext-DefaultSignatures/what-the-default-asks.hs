{-# LANGUAGE DefaultSignatures #-}
-- An instance that takes the default must have what the default asks
class Describe a where
  describe :: a -> String
  default describe :: Show a => a -> String
  describe = show

newtype Secret = Secret Int

instance Describe Secret

main :: IO ()
main = putStrLn (describe (Secret 1))
