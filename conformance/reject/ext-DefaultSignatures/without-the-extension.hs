-- DefaultSignatures is not the Report's: without the extension this is refused
class Describe a where
  describe :: a -> String
  default describe :: Show a => a -> String
  describe = show

main :: IO ()
main = return ()
