-- InstanceSigs is not the Report's: without the extension an instance cannot give a method a type signature
data T = T

instance Show T where
  show :: T -> String
  show T = "T"

main :: IO ()
main = return ()
