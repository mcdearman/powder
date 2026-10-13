-- DeriveDataTypeable is not the Report's: without the extension this is refused
import Data.Typeable (Typeable)

data Colour = Red | Green deriving (Typeable)

main :: IO ()
main = return ()
