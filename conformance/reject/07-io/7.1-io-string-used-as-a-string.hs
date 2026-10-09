-- Haskell 2010 Report 7.1 / 7.2: getLine :: IO String is an action, not a String; its result must be bound with <- or >>=
main :: IO ()
main = putStrLn ("got: " ++ getLine)
