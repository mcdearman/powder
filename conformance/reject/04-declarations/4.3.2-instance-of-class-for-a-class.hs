-- Haskell 2010 Report 4.3.2: an instance is for a type constructor; a class is not one
class MyClass a

instance Eq MyClass

main :: IO ()
main = return ()
