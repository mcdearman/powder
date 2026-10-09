-- Haskell 2010 Report 4.3.2, 5.3: imports come at the head of a module, not among an instance's
-- declarations
data T = T

instance Show T where
  import Data.List

main :: IO ()
main = return ()
