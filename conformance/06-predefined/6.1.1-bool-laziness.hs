-- Haskell 2010 Report 6.1.1: (&&) and (||) do not evaluate their second argument when the first decides
bottom :: Bool
bottom = undefined

myAnd :: Bool -> Bool -> Bool
myAnd True x = x
myAnd False _ = False

pick :: Int -> String
pick n
  | n == 0 || bottom = "zero"
  | otherwise = "other"

main :: IO ()
main = do
  print ("False && bottom", False && bottom)
  print ("True || bottom", True || bottom)
  print ("chain and", True && False && bottom)
  print ("chain or", False || True || bottom)
  print ("mixed", False && bottom || True)
  print ("user and", myAnd False bottom)
  print ("and partial list", and (True : False : undefined))
  print ("or partial list", or (False : True : undefined))
  print ("any infinite", any even [(1 :: Int) ..])
  print ("all infinite", all (< 10) [(1 :: Int) ..])
  print ("elem infinite", 7 `elem` [(1 :: Int) ..])
  print ("if short circuit", if True || bottom then "yes" else "no")
  print ("guard short circuit", pick 0)
  print ("not forced in tuple", fst (True, bottom))
  print ("foldr and", foldr (&&) True (cycle [True, False]))
