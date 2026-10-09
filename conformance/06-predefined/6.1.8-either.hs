-- Haskell 2010 Report 6.1.8: Either, the either function, and its Eq, Ord, Show, Read instances
type E = Either Int Char

parseDigit :: Char -> Either String Int
parseDigit c
  | c >= '0' && c <= '9' = Right (fromEnum c - fromEnum '0')
  | otherwise = Left ("not a digit: " ++ [c])

line :: Show a => String -> a -> IO ()
line label x = putStrLn (label ++ ": " ++ show x)

main :: IO ()
main = do
  print ("constructors", Left 1 :: E, Right 'a' :: E)
  print ("either", either show (map succ) (Left 3 :: Either Int String), either show (map succ) (Right "abc" :: Either Int String))
  print ("either 2", either length negate (Left "four" :: Either String Int), either length negate (Right 5 :: Either String Int))
  print ("either with constructors", either Right Left (Left 'x' :: Either Char Bool))
  print ("eq", (Left 1 :: E) == Left 1, (Left 1 :: E) /= Left 2, (Left 1 :: E) == Right 'a', (Right 'a' :: E) == Right 'a')
  print ("ord Left < Right", (Left 9 :: Either Int Int) < Right 0, compare (Right 'a' :: E) (Left 100))
  print ("ord within", compare (Left 1 :: E) (Left 2), compare (Right 'b' :: E) (Right 'a'), max (Left 5 :: E) (Left 3))
  line "show Left" (Left 1 :: E)
  line "show Left negative" (Left (-1) :: E)
  line "show Right" (Right 'a' :: E)
  line "show nested" (Right (Left 2) :: Either Bool (Either Int Bool))
  line "show with Maybe" (Left (Just 'x') :: Either (Maybe Char) ())
  line "show list" [Left 1, Right 'b' :: E]
  putStrLn ("showsPrec 11: " ++ showsPrec 11 (Left 1 :: E) "")
  print ("read", read "Left 1" :: E, read "Right 'x'" :: E, read " ( Right 'x' ) " :: E, read "Left (-5)" :: E)
  print ("read nested", read "Right (Left 2)" :: Either Bool (Either Int Bool))
  print ("reads failure", reads "Middle 1" :: [(E, String)], reads "Left 'c'" :: [(E, String)])
  print ("parse", parseDigit '7', parseDigit 'x')
  print ("lefts and rights", [x | Left x <- [Left 1, Right 'a', Left 2 :: E]], [y | Right y <- [Left 1, Right 'a' :: E]])
  print ("case", case parseDigit '4' of { Left msg -> length msg; Right n -> n * 2 })
