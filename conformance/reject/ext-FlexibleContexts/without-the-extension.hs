-- FlexibleContexts is not the Report's: without the extension a class is asked of a variable, or one applied to types
f :: Show [a] => a -> String
f x = show [x]

main :: IO ()
main = return ()
