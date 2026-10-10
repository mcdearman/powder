-- Haskell 2010 Report 4.3.1: a class's context names classes; Int is a type
class (Int x) => Super x where
  member :: x

main :: IO ()
main = return ()
