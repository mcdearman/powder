-- RankNTypes is not the Report's: without the extension a type has no forall inside it
both :: (forall a. a -> a) -> (Int, Bool) -> (Int, Bool)
both f (n, b) = (f n, f b)

main :: IO ()
main = return ()
