-- Haskell 2010 Report 9: maybe and either
module Main (main) where

describe :: Either Int String -> String
describe = either (\n -> "number " ++ show n) (\s -> "text " ++ s)

main :: IO ()
main = do
  print ("maybe Nothing", maybe 0 (+ 1) (Nothing :: Maybe Int))
  print ("maybe Just", maybe 0 (+ 1) (Just 5 :: Maybe Int))
  print ("maybe changes type", maybe "none" show (Just (42 :: Int)), maybe "none" show (Nothing :: Maybe Int))
  print ("maybe with const", maybe False (const True) (Just 'x'), maybe False (const True) (Nothing :: Maybe Char))
  print ("maybe default not forced", maybe (undefined :: Int) (* 2) (Just 4))
  print ("maybe function not forced", maybe 'd' (undefined :: Int -> Char) Nothing)
  print ("maybe argument not forced", maybe 0 (const 1) (Just (undefined :: Int)) :: Int)
  print ("maybe partial", map (maybe (-1) id) [Just 1, Nothing, Just (3 :: Int)])
  print ("maybe nested", maybe 0 (maybe 1 (+ 2)) (Just (Just (10 :: Int))), maybe 0 (maybe 1 (+ 2)) (Just (Nothing :: Maybe Int)))
  print ("either Left", either (+ 1) length (Left 5 :: Either Int String))
  print ("either Right", either (+ 1) length (Right "abc" :: Either Int String))
  print ("either describe", describe (Left 3), describe (Right "hi"))
  print ("either id", either id negate (Left 1 :: Either Int Int), either id negate (Right 1 :: Either Int Int))
  print ("either other branch not forced", either (undefined :: Int -> Int) (+ 1) (Right 1), either (+ 1) (undefined :: Int -> Int) (Left 1))
  print ("either payload not forced", either (const 'l') (const 'r') (Left (undefined :: Int) :: Either Int Int))
  print ("either map", map (either show id) [Left (1 :: Int), Right "two", Left 3])
  print ("either to Maybe", either (const Nothing) Just (Right 'x' :: Either Int Char), either (const Nothing) Just (Left 0 :: Either Int Char))
  print ("either rebuild", either Left Right (Left 1 :: Either Int Bool), either Right Left (Left 1 :: Either Int Int))
