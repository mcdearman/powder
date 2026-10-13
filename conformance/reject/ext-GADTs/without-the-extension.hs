{-# LANGUAGE GADTSyntax #-}
-- GADTs is not the Report's: without the extension a constructor makes its type at the type's own variables
data Expr a where
  IntE :: Int -> Expr Int

main :: IO ()
main = return ()
