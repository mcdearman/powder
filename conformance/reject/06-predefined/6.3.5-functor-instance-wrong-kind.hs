-- Haskell 2010 Report 6.3.5: Functor is a class over type constructors of kind * -> *; Bool has kind *
instance Functor Bool where
  fmap _ b = b

main :: IO ()
main = print True
