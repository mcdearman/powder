-- Haskell 2010 Report 4.3.1, 5.3: imports come at the head of a module, not among a class's declarations
class Class a where
  import Data.List

main :: IO ()
main = return ()
