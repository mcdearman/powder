-- Haskell 2010 Report 6.1.8: Maybe, the maybe function, and its Eq, Ord, Show, Read, Functor, Monad instances
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv a b = Just (a `div` b)

line :: Show a => String -> a -> IO ()
line label x = putStrLn (label ++ ": " ++ show x)

main :: IO ()
main = do
  print ("constructors", Just 'a', Nothing :: Maybe Char)
  print ("maybe", maybe 0 (+ 1) (Just (5 :: Int)), maybe 0 (+ 1) (Nothing :: Maybe Int))
  print ("maybe to string", maybe "none" show (Just True), maybe "none" show (Nothing :: Maybe Bool))
  print ("maybe lazy default", maybe undefined not (Just False))
  print ("eq", Just 'a' == Just 'a', Just 'a' /= Just 'b', Nothing == (Nothing :: Maybe Int), Just 'a' == Nothing)
  print ("ord", Nothing < Just 'a', Just 'a' < Just 'b', compare (Just 'b') (Just 'a'), compare (Just 'a') Nothing)
  print ("ord 2", max Nothing (Just 'x'), min Nothing (Just 'x'), Just (Just 'a') > Just Nothing)
  line "show Just" (Just (3 :: Int))
  line "show Just negative" (Just (-3 :: Int))
  line "show nested" (Just (Just (3 :: Int)))
  line "show Just Nothing" (Just (Nothing :: Maybe Int))
  line "show Just string" (Just "s")
  line "show Just list" (Just [1, 2 :: Int])
  line "show Just tuple" (Just (1 :: Int, 'c'))
  line "show list of Maybe" [Just (1 :: Int), Nothing]
  putStrLn ("showsPrec 11: " ++ showsPrec 11 (Just 'a') "" ++ " " ++ showsPrec 11 (Nothing :: Maybe Char) "")
  putStrLn ("showsPrec 10: " ++ showsPrec 10 (Just 'a') "")
  print ("read", read "Just 3" :: Maybe Int, read " Just  3 " :: Maybe Int, read "(Just 3)" :: Maybe Int)
  print ("read 2", read "Just (-3)" :: Maybe Int, read "Nothing" :: Maybe Int, read "Just (3)" :: Maybe Int)
  print ("read nested", read "Just (Just 4)" :: Maybe (Maybe Int), read "Just Nothing" :: Maybe (Maybe Int))
  print ("reads failures", reads "Just" :: [(Maybe Int, String)], reads "Just Just 3" :: [(Maybe (Maybe Int), String)])
  print ("reads rest", reads "Nothing more" :: [(Maybe Int, String)])
  print ("fmap", fmap (+ 1) (Just (1 :: Int)), fmap (+ 1) (Nothing :: Maybe Int), fmap show (Just True))
  print ("bind", Just 12 >>= safeDiv 144, Just 0 >>= safeDiv 1, Nothing >>= safeDiv 1)
  print ("then", Just 'a' >> Just 'b', (Nothing :: Maybe Char) >> Just 'b', Just 'a' >> (Nothing :: Maybe Char))
  print ("return", return 'r' :: Maybe Char)
  print ("do", do { a <- safeDiv 100 5; b <- safeDiv a 2; return (a + b) })
  print ("do failing", do { a <- safeDiv 100 0; b <- safeDiv a 2; return (a + b) })
