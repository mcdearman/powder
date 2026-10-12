{-# LANGUAGE MultiParamTypeClasses #-}
-- FunctionalDependencies is not the Report's: without the extension this is refused
class Container f a | f -> a where
  empty :: f

main :: IO ()
main = return ()
