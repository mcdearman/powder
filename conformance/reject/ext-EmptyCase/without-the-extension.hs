-- EmptyCase is not the Report's: without the extension this is refused
f x = case x of {}

main :: IO ()
main = return ()
