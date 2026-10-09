-- Haskell 2010 Report 11.4: derived showsPrec d for nullary constructors (never parenthesised) and prefix constructor
-- applications (parenthesised exactly when d > 10; arguments are shown at precedence 11), for every d from 0 to 11
module Main (main) where

data Color = Red | Green | Blue deriving (Show)

data T = N | U Int | B Int Int | W T | V T T | M (Maybe T) deriving (Show)

-- one line per precedence
table :: Show a => String -> a -> IO ()
table label x = mapM_ (\d -> putStrLn (label ++ " @" ++ show d ++ ": " ++ showsPrec d x "")) [0 .. 11 :: Int]

main :: IO ()
main = do
  table "Red" Red
  table "N" N
  table "U 1" (U 1)
  table "B 1 2" (B 1 2)
  table "W N" (W N)
  table "W (U 1)" (W (U 1))
  table "W (W (W N))" (W (W (W N)))
  table "V (U 1) (B 2 3)" (V (U 1) (B 2 3))
  table "V N N" (V N N)
  table "M Nothing" (M Nothing)
  table "M (Just (U 1))" (M (Just (U 1)))
  table "Just Red" (Just Red)
  table "Just (U 1)" (Just (U 1))
  table "[U 1,N]" [U 1, N]
  table "(U 1,N)" (U 1, N)
  putStrLn (show Red ++ " " ++ show Green ++ " " ++ show Blue)
  putStrLn (show [Red, Green, Blue] ++ " " ++ show (Red, Blue) ++ " " ++ show (Just Green) ++ " " ++ show [Just Blue])
  putStrLn (show (Left Red :: Either Color T) ++ " " ++ show (Right (U 5) :: Either Color T))
  putStrLn (shows (U 1) " tail" ++ " | " ++ showsPrec 11 (U 1) " tail" ++ " | " ++ showsPrec 11 N " tail")
  putStrLn (showsPrec 12 (U 1) "" ++ " | " ++ showsPrec 100 N "" ++ " | " ++ showsPrec (-5) (U 1) "")
  print (V (W (V N (U 1))) (M (Just (M Nothing))))
