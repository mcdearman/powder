-- Haskell 2010 Report 11.4: derived readsPrec via reads: a successful parse returns the remaining input, a failed
-- parse returns [] (reads never raises an error)
module Main (main) where

data Color = Red | Green | Blue deriving (Eq, Show, Read)

data T = N | U Int | B Int Int | W T deriving (Eq, Show, Read)

data P = P Int Double deriving (Eq, Show, Read)

rc :: String -> [(Color, String)]
rc = reads

rt :: String -> [(T, String)]
rt = reads

rp :: String -> [(P, String)]
rp = reads

main :: IO ()
main = do
  print ("nullary whole", rc "Red")
  print ("nullary remaining", rc "Red rest", rc "Green Blue", rc "Blue,Red", rc "Red)")
  print ("nullary leading space, trailing kept", rc "  Red  ")
  print ("nullary parenthesised", rc "(Red) x", rc "((Green))y")
  print ("nullary failures", rc "", rc "   ", rc "red", rc "Redx", rc "Re", rc "R ed")
  print ("nullary failures 2", rc "(Red", rc ")Red", rc "()", rc "1", rc "\"Red\"", rc "'R'")
  print ("null on failure", null (rc "Purple"), null (rc "Red"), length (rc "Red"))
  print ("prefix whole", rt "U 1", rt "B 1 2", rt "N")
  print ("prefix remaining", rt "U 1 2", rt "B 1 2 3", rt "N N", rt "U 1)", rt "B 1 2,")
  print ("prefix remaining text", rt "U 5 and the rest", rt "W N tail", rt "W (U 1) tail")
  print ("prefix parenthesised", rt "(U 1) x", rt "(B 1 2)(", rt "((N)) N")
  print ("prefix failures: too few arguments", rt "U", rt "B 1", rt "B", rt "W")
  print ("prefix failures: wrong argument", rt "U x", rt "U N", rt "B 1 x", rt "U 'c'", rt "U \"s\"")
  print ("prefix failures: unparenthesised nested application", rt "W U 1", rt "W W N")
  print ("prefix failures: case and spelling", rt "u 1", rt "UU 1", rt "U1", rt "n")
  print ("prefix failures: parentheses", rt "(U 1", rt "(U) 1", rt "U (1", rt "()")
  print ("double field", rp "P 1 2.5 x", rp "P (-1) (-2.5)", rp "P 1 2")
  print ("double field failures", rp "P 1", rp "P 1.5 2", rp "P x 1.5")
  print ("in structures", reads "[Red,Green] x" :: [([Color], String)], reads "(Red,U 1) x" :: [((Color, T), String)], reads "Just Blue x" :: [(Maybe Color, String)])
  print ("in structures failures", reads "[Red,Gren]" :: [([Color], String)], reads "(Red,U)" :: [((Color, T), String)], reads "Just Purple" :: [(Maybe Color, String)])
  print ("chained reads", [(a, b, r2) | (a, r1) <- rc "Red U 7 end", (b, r2) <- rt r1])
  print ("safe read", map (\s -> case rc s of { [(c, r)] | all (== ' ') r -> Just c; _ -> Nothing }) ["Red", " Blue ", "Bleu", "Red Green", ""])
