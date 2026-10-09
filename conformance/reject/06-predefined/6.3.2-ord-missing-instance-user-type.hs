-- Haskell 2010 Report 6.3.2: (<) needs an Ord instance; this type derives only Eq and Show
data Size = Small | Large deriving (Eq, Show)

main :: IO ()
main = print (Small < Large)
