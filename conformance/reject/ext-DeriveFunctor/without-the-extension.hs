-- DeriveFunctor is not the Report's: without the extension this is refused
data Box a = Box a deriving (Functor)

main :: IO ()
main = return ()
