-- MultiParamTypeClasses is not the Report's: without the extension a class has one type variable
class Convert a b where
  convert :: a -> b

main :: IO ()
main = return ()
