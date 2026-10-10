-- Haskell 2010 Report 4.3.1: a default method must have the type the class gives the method; 1 is not of
-- every type
class HasDefault a where
  value :: a
  value = 1

instance HasDefault Bool

main :: IO ()
main = print (value :: Bool)
