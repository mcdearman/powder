{-# LANGUAGE Haskell2010, KindSignatures #-}
-- PolyKinds is not the Report's: without the extension a kind has no variable in it
data Proxy (a :: k) = Proxy

main :: IO ()
main = return ()
