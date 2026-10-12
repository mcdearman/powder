-- TypeOperators is not the Report's: without the extension a type is not written with an operator
data Sum a b = L a | R b

f :: Int `Sum` Bool -> Int
f _ = 0

main :: IO ()
main = return ()
