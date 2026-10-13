-- ExistentialQuantification is not the Report's: without the extension this is refused
data Showable = forall a. Show a => MkShowable a

main :: IO ()
main = return ()
