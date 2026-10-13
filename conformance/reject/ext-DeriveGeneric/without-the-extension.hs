-- DeriveGeneric is not the Report's: without the extension this is refused
import GHC.Generics (Generic)

data Colour = Red | Green deriving (Generic)

main :: IO ()
main = return ()
