-- DerivingStrategies is not the Report's: without the extension this is refused
data T = T deriving stock (Show)

main :: IO ()
main = return ()
