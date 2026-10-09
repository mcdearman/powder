-- Haskell 2010 Report 6.3.6: (>>=) :: m a -> (a -> m b) -> m b keeps the same monad m; Maybe cannot be bound into a list
main :: IO ()
main = print (Just (1 :: Int) >>= \x -> [x])
