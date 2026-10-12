-- GeneralisedNewtypeDeriving is not the Report's: without the extension a newtype does not get Num by deriving
newtype Age = Age Int deriving (Eq, Show, Num)

main :: IO ()
main = return ()
