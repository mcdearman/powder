-- FlexibleInstances is not the Report's: without the extension an instance is for a constructor applied to variables
class C a where
  c :: a -> Int

instance C [Char] where
  c = length

main :: IO ()
main = return ()
