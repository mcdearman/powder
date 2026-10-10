-- NamedFieldPuns is not the Report's: without the extension this is refused
data P = P {x :: Int}
f P {x} = x

main :: IO ()
main = return ()
