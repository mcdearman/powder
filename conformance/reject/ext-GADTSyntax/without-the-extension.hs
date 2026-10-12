-- GADTSyntax is not the Report's: without the extension this is refused
data T a where
  C :: a -> T a

main :: IO ()
main = return ()
