-- Haskell 2010 Report 6.3.3: reads returns [] when no parse exists (read would be an error); checked without calling read
readsInt :: String -> [(Int, String)]
readsInt = reads

readsBool :: String -> [(Bool, String)]
readsBool = reads

readsChar :: String -> [(Char, String)]
readsChar = reads

readsString :: String -> [(String, String)]
readsString = reads

readsList :: String -> [([Int], String)]
readsList = reads

readsPair :: String -> [((Int, Bool), String)]
readsPair = reads

readsMaybe :: String -> [(Maybe Int, String)]
readsMaybe = reads

-- the definition of read from the Report, returning Nothing instead of calling error
readMaybeInt :: String -> Maybe Int
readMaybeInt s = case [x | (x, t) <- readsInt s, ("", "") <- lex t] of
  [x] -> Just x
  _ -> Nothing

main :: IO ()
main = do
  print ("Int empty", readsInt "", readsInt "   ")
  print ("Int from letters", readsInt "abc", readsInt "True", readsInt "x1")
  print ("Int from fraction", readsInt "1.5")
  print ("Int unbalanced", readsInt "(1", readsInt "((1)")
  print ("Int then close paren", readsInt "1)")
  print ("Int from unit or list", readsInt "()", readsInt "[1]")
  print ("Bool", readsBool "true", readsBool "Tru", readsBool "Truex", readsBool "1", readsBool "")
  print ("Bool ok for contrast", readsBool "True", readsBool "False!")
  print ("Char", readsChar "a", readsChar "'ab'", readsChar "''", readsChar "\"a\"", readsChar "'a")
  print ("String", readsString "abc", readsString "\"unterminated", readsString "'a'")
  print ("list", readsList "[1,2", readsList "[1,,2]", readsList "[1,2,]", readsList "1,2", readsList "[1 2]")
  print ("list of wrong element", readsList "[True]", readsList "['a']")
  print ("pair", readsPair "(1)", readsPair "(1,True", readsPair "(1,True,2)", readsPair "(True,1)", readsPair "1,True")
  print ("Maybe", readsMaybe "Just", readsMaybe "Just True", readsMaybe "just 1", readsMaybe "Nothin")
  print ("Maybe argument needs parens when compound", reads "Just Just 1" :: [(Maybe (Maybe Int), String)])
  print ("Ordering", reads "lt" :: [(Ordering, String)], reads "LE" :: [(Ordering, String)])
  print ("Either", reads "Left" :: [(Either Int Int, String)], reads "Centre 1" :: [(Either Int Int, String)])
  print ("unit", reads "(" :: [((), String)], reads ")" :: [((), String)], reads "unit" :: [((), String)])
  print ("total read ok", readMaybeInt "12", readMaybeInt " 12 ", readMaybeInt "(12)", readMaybeInt "-12")
  print ("total read fails", readMaybeInt "12 13", readMaybeInt "12x", readMaybeInt "", readMaybeInt "twelve", readMaybeInt "1.0")
