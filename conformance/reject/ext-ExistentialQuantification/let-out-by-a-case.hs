{-# LANGUAGE ExistentialQuantification #-}
-- Nor what a case gives
data Hidden = forall a. Show a => Hidden a

main :: IO ()
main = do
  let y = case Hidden 'x' of Hidden x -> x
  y `seq` return ()
