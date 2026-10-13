-- DerivingVia is not the Report's: without the extension this is refused
newtype Wrapper = Wrapper Int

instance Show Wrapper where
  show _ = "w"

newtype Total = Total Int deriving (Show) via Wrapper

main :: IO ()
main = return ()
