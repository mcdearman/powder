-- OverloadedStrings is not the Report's: without the extension a string is a String and nothing else
newtype Name = Name String

class FromText a where
  fromText :: String -> a

n :: Name
n = "ann"

main :: IO ()
main = return ()
