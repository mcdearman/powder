-- Haskell 2010 Report 6.1.1: Bool, (&&), (||), not, otherwise and Bool's standard instances
classify :: Int -> String
classify n
  | n > 0 = "positive"
  | n < 0 = "negative"
  | otherwise = "zero"

main :: IO ()
main = do
  print ("and table", [a && b | a <- [False, True], b <- [False, True]])
  print ("or table", [a || b | a <- [False, True], b <- [False, True]])
  print ("not", not True, not False)
  print ("otherwise", otherwise)
  print ("and binds tighter than or", True || False && False)
  print ("and binds tighter than or 2", False && False || True)
  print ("not is ordinary application", not True || True)
  print ("comparison binds tighter", 1 < (2 :: Int) && 'a' < 'b')
  print ("bounds", minBound :: Bool, maxBound :: Bool)
  print ("order", False < True, compare True False, max False True)
  print ("eq", True == True, True /= False, False == True)
  print ("enum", [False ..], fromEnum True, fromEnum False)
  print ("toEnum", toEnum 0 :: Bool, toEnum 1 :: Bool)
  print ("show", show True, show False, showsPrec 11 True "")
  print ("read", read "False" :: Bool, read " True " :: Bool)
  print ("guards", classify 5, classify (-5), classify 0)
  print ("if", if not False then 'y' else 'n')
