{-# LANGUAGE ExistentialQuantification #-}
-- What a constructor keeps to itself has a type known only where it is matched: it is not what the match gives
data Hidden = forall a. Hidden a

reveal (Hidden x) = x

main :: IO ()
main = return ()
