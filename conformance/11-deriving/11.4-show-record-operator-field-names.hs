-- Haskell 2010 Report 11.4: derived Show for record fields whose names are operators: the field name is shown in
-- parentheses, `C {(%%) = 1}`
module Main (main) where

data C = C {(%%) :: Int} deriving (Show)

data D = D {(<+>) :: Int, plain :: Char, (|>) :: Maybe Int} deriving (Show)

data P a = P {(.:) :: a, (+++) :: [a]} deriving (Show)

main :: IO ()
main = do
  print (C 1)
  print (C (-1))
  print (C {(%%) = 5})
  print (Just (C 1))
  print [C 1, C 2]
  putStrLn (showsPrec 10 (C 1) "" ++ " | " ++ showsPrec 11 (C 1) "")
  print (D 1 'c' Nothing)
  print (D {(|>) = Just (-3), plain = 'p', (<+>) = -2})
  print (Just (D 0 'z' (Just 0)))
  print (P 'a' "bc")
  print (P (-1.5 :: Double) [-2.5])
  print (P (C 1) [C 2, C 3])
  print (P {(+++) = [Just True], (.:) = Nothing})
  print ((%%) (C 9), (<+>) (D 1 'c' Nothing), (.:) (P 'q' ""))
  print ((C 1) {(%%) = 2})
