-- Haskell 2010 Report 9: Prelude types Bool, Maybe, Either, Ordering with their constructors and instances
module Main (main) where

boolName :: Bool -> String
boolName False = "no"
boolName True = "yes"

fromM :: Maybe Int -> Int
fromM Nothing = -1
fromM (Just n) = n

side :: Either Int String -> String
side (Left n) = "L" ++ show n
side (Right s) = "R" ++ s

ordChar :: Ordering -> Char
ordChar LT = '<'
ordChar EQ = '='
ordChar GT = '>'

main :: IO ()
main = do
  print ("bool match", boolName False, boolName True)
  print ("bool show", False, True)
  print ("bool eq", False == False, False == True, True /= False)
  print ("bool ord", False < True, compare True False, max False True, min False True)
  print ("bool bounded", minBound :: Bool, maxBound :: Bool)
  print ("bool enum", [False ..], fromEnum True, toEnum 0 :: Bool)
  print ("bool read", read "True" :: Bool, read "False" :: Bool)
  print ("maybe match", fromM Nothing, fromM (Just 7))
  print ("maybe show", Nothing :: Maybe Int, Just 3 :: Maybe Int, Just (Just 'x'))
  print ("maybe eq", Just 1 == Just (1 :: Int), Just 1 == (Nothing :: Maybe Int), Nothing == (Nothing :: Maybe Int))
  print ("maybe ord", Nothing < Just (0 :: Int), compare (Just 2) (Just (1 :: Int)), Just (minBound :: Int) > Nothing)
  print ("maybe read", read "Nothing" :: Maybe Int, read "Just 4" :: Maybe Int)
  print ("either match", side (Left 3), side (Right "x"))
  print ("either show", Left 1 :: Either Int Bool, Right True :: Either Int Bool)
  print ("either eq", (Left 1 :: Either Int Int) == Left 1, (Left 1 :: Either Int Int) == Right 1)
  print ("either ord", (Left 9 :: Either Int Int) < Right 0, compare (Right 'a') (Left 'b' :: Either Char Char))
  print ("either read", read "Left 5" :: Either Int Bool, read "Right False" :: Either Int Bool)
  print ("ordering match", map ordChar [LT, EQ, GT])
  print ("ordering show", LT, EQ, GT)
  print ("ordering eq/ord", LT == LT, LT < EQ, EQ < GT, compare GT LT, maximum [EQ, LT, GT])
  print ("ordering bounded", minBound :: Ordering, maxBound :: Ordering)
  print ("ordering enum", [LT ..], fromEnum GT, toEnum 1 :: Ordering)
  print ("ordering read", read "EQ" :: Ordering)
