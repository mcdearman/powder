-- EmptyDataDeriving is not the Report's: without the extension this is refused
data Void deriving (Eq)

main :: IO ()
main = return ()
