-- ExplicitForAll is not the Report's: without the extension forall is a type variable like any other
identity :: forall a. a -> a
identity x = x

main :: IO ()
main = return ()
