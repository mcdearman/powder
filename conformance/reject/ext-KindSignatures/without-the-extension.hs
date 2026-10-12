-- KindSignatures is not the Report's: without the extension a type variable cannot be given a kind
data Wrap (f :: * -> *) a = Wrap (f a)

main :: IO ()
main = return ()
