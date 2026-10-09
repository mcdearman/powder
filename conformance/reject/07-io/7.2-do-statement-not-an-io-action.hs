-- Haskell 2010 Report 7.2: every statement of an IO do block must be an IO action; length "abc" is an Int
main :: IO ()
main = do
  length "abc"
  putStrLn "x"
