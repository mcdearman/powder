-- Haskell 2010 Report 9 (PreludeText): user Show instances; defaults of show/showsPrec/showList and overriding showList
module Main (main) where

-- showsPrec only: show and showList come from the defaults
data V2 = V2 Int Int

instance Show V2 where
  showsPrec d (V2 x y) = showParen (d > 10) (showString "V2 " . showsPrec 11 x . showChar ' ' . showsPrec 11 y)

-- show only: showsPrec comes from the default (showsPrec _ x s = show x ++ s), so no parentheses are ever added
data Colour = Red | Green

instance Show Colour where
  show Red = "red"
  show Green = "green"

-- showList overridden, in the same way the Prelude does it for Char
newtype Bit = Bit Bool

instance Show Bit where
  showsPrec _ (Bit b) = showChar (if b then '1' else '0')
  showList bs = showString "<" . foldr (\b acc -> shows b . acc) id bs . showString ">"

-- an infix operator shown with its own precedence
data Frac = Int :/ Int

instance Show Frac where
  showsPrec d (n :/ m) = showParen (d > 7) (showsPrec 8 n . showString " :/ " . showsPrec 8 m)

main :: IO ()
main = do
  putStrLn (show (V2 1 2))
  putStrLn (show (V2 (-1) 2))
  putStrLn (show (Just (V2 1 (-2))))
  putStrLn (show [V2 1 2, V2 3 4])
  putStrLn (show ([] :: [V2]))
  putStrLn (show (V2 1 2, V2 3 4))
  putStrLn (showsPrec 10 (V2 1 2) "" ++ " | " ++ showsPrec 11 (V2 1 2) "" ++ " | " ++ shows (V2 1 2) " tail")
  putStrLn (show (Left (V2 0 0) :: Either V2 Int))
  putStrLn (show Red ++ " " ++ show Green)
  putStrLn (showsPrec 11 Red "" ++ " " ++ shows Green "!")
  putStrLn (show [Red, Green, Red])
  putStrLn (show (Just Red) ++ " " ++ show (Red, Green) ++ " " ++ show [Just Green])
  putStrLn (showList [Red, Green] " tail")
  putStrLn (show (Bit True) ++ " " ++ show (Bit False))
  putStrLn (show [Bit True, Bit False, Bit True])
  putStrLn (show ([] :: [Bit]))
  putStrLn (show [[Bit True], [], [Bit False, Bit False]])
  putStrLn (show (Just [Bit True, Bit True]) ++ " " ++ show (Bit True, [Bit False]) ++ " " ++ show [Just (Bit True)])
  putStrLn (showList [Bit True] "" ++ " " ++ shows [Bit False] "tail" ++ " " ++ showsPrec 11 [Bit True, Bit False] "")
  putStrLn (show (1 :/ 2) ++ " | " ++ show (Just (1 :/ 2)) ++ " | " ++ show ((-1) :/ 2) ++ " | " ++ show [1 :/ 2, 3 :/ 4])
  putStrLn (showsPrec 7 (1 :/ 2) "" ++ " | " ++ showsPrec 8 (1 :/ 2) "")
  print (V2 5 6)
  print [Bit True]
  print (Just Green)
