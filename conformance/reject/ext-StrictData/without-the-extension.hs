-- StrictData is not the Report's: without the extension a field cannot be marked lazy
data T = T ~Int

main :: IO ()
main = return ()
