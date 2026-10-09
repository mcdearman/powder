-- Haskell 2010 Report 7.2: the type of a do block is the type of its last expression; IO Char is not IO ()
main :: IO ()
main = do
  putStrLn "a"
  return 'x'
