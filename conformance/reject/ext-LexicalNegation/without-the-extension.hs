-- LexicalNegation is not the Report's: without the extension a minus before an argument is an operator
main :: IO ()
main = print (abs -3 :: Int)
