-- Haskell 2010 Report 6.3.3 / 6.1.6: function types are not instances of Show
increment :: Int -> Int
increment x = x + 1

main :: IO ()
main = print increment
