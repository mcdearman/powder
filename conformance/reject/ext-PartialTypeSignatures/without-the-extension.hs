-- PartialTypeSignatures is not the Report's: without the extension a type cannot have a part left out
f :: _ -> Int
f x = x + 1

main :: IO ()
main = return ()
