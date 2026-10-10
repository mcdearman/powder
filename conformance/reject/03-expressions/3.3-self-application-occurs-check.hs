-- Haskell 2010 Report 4.5.2: a lambda-bound variable has one type, which cannot be a function from itself
selfApply x = x x

main :: IO ()
main = return ()
