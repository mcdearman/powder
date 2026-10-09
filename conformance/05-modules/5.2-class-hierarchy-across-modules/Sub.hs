module Sub (Sub(..), viaSub) where

import Super

class Super a => Sub a where
  extra :: a -> String
  extra x = "default" ++ show (base x)

instance Sub a => Sub [a] where
  extra = concatMap extra

viaSub :: Sub a => a -> String
viaSub x = extra x ++ "/" ++ show (twiceBase x)
