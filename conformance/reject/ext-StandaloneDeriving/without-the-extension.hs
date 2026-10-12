-- StandaloneDeriving is not the Report's: without the extension this is refused
data T = T

deriving instance Show T

main :: IO ()
main = return ()
