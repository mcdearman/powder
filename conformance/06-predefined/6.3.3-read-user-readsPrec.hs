-- Haskell 2010 Report 6.3.3: user Read instances via readsPrec (with lex and readParen); default readList
data Colour = Red | Green | Blue deriving (Eq, Show)

instance Read Colour where
  readsPrec _ s =
    [(Red, r) | ("red", r) <- lex s]
      ++ [(Green, r) | ("green", r) <- lex s]
      ++ [(Blue, r) | ("blue", r) <- lex s]

data Pt = Pt Int Int deriving (Eq, Show)

instance Read Pt where
  readsPrec d =
    readParen
      (d > 10)
      (\r -> [(Pt x y, u) | ("Pt", s) <- lex r, (x, t) <- readsPrec 11 s, (y, u) <- readsPrec 11 t])

-- a parameterised type: "<a|b>"
data Duo a = Duo a a deriving (Eq, Show)

instance Read a => Read (Duo a) where
  readsPrec _ r =
    [ (Duo x y, r5)
    | ("<", r1) <- lex r
    , (x, r2) <- reads r1
    , ("|", r3) <- lex r2
    , (y, r4) <- reads r3
    , (">", r5) <- lex r4
    ]

main :: IO ()
main = do
  print ("colour", read "red" :: Colour, read " green " :: Colour, read "blue" :: Colour)
  print ("colour list via default readList", read "[red,green,blue]" :: [Colour], read " [ red , blue ] " :: [Colour])
  print ("colour in tuple and Maybe", read "(red,green)" :: (Colour, Colour), read "Just blue" :: Maybe Colour)
  print ("colour failures", reads "purple" :: [(Colour, String)], reads "Red" :: [(Colour, String)])
  print ("colour reads rest", reads "red green" :: [(Colour, String)])
  print ("pt", read "Pt 1 2" :: Pt, read "(Pt 1 2)" :: Pt, read " Pt  3   4 " :: Pt, read "Pt (-1) 2" :: Pt)
  print ("pt nested", read "Just (Pt 1 2)" :: Maybe Pt, read "[Pt 1 2,Pt 3 4]" :: [Pt], read "(Pt 1 2,True)" :: (Pt, Bool))
  print ("pt needs parens under a constructor", reads "Just Pt 1 2" :: [(Maybe Pt, String)])
  print ("pt precedence", readsPrec 10 "Pt 1 2" :: [(Pt, String)], readsPrec 11 "Pt 1 2" :: [(Pt, String)])
  print ("pt precedence with parens", readsPrec 11 "(Pt 1 2)" :: [(Pt, String)])
  print ("pt failures", reads "Pt 1" :: [(Pt, String)], reads "Pt a b" :: [(Pt, String)], reads "pt 1 2" :: [(Pt, String)])
  print ("duo", read "<1|2>" :: Duo Int, read " < True | False > " :: Duo Bool, read "<red|blue>" :: Duo Colour)
  print ("duo nested", read "< <1|2> | <3|4> >" :: Duo (Duo Int), read "[<'a'|'b'>]" :: [Duo Char])
  print ("duo failures", reads "<1|>" :: [(Duo Int, String)], reads "<1,2>" :: [(Duo Int, String)])
  print ("lex", lex " hello world", lex "(x)", lex "<=3", lex "12 rest", lex "")
