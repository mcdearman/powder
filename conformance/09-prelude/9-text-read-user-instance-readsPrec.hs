-- Haskell 2010 Report 9 (PreludeText): user Read instances written with readsPrec, readParen and lex; default readList
module Main (main) where

data Point = Point Int Int

instance Show Point where
  showsPrec d (Point x y) = showParen (d > 10) (showString "Point " . showsPrec 11 x . showChar ' ' . showsPrec 11 y)

instance Read Point where
  readsPrec d = readParen (d > 10) (\r -> [(Point x y, u) | ("Point", s) <- lex r, (x, t) <- readsPrec 11 s, (y, u) <- readsPrec 11 t])

-- an infix form "n / m" at precedence 7
data Q = Q Int Int

instance Show Q where
  showsPrec d (Q n m) = showParen (d > 7) (showsPrec 8 n . showString " / " . showsPrec 8 m)

instance Read Q where
  readsPrec d = readParen (d > 7) (\r -> [(Q n m, u) | (n, s) <- readsPrec 8 r, ("/", t) <- lex s, (m, u) <- readsPrec 8 t])

-- a keyword-only type with a custom concrete syntax
data Switch = On | Off

instance Show Switch where
  show On = "on"
  show Off = "off"

instance Read Switch where
  readsPrec _ r = [(On, s) | ("on", s) <- lex r] ++ [(Off, s) | ("off", s) <- lex r]

main :: IO ()
main = do
  print ("Point", read "Point 1 2" :: Point, read "  Point   3   4  " :: Point)
  print ("Point parens", read "(Point 1 2)" :: Point, read " ( ( Point 1 2 ) ) " :: Point)
  print ("Point negative fields", read "Point (-1) (-2)" :: Point, read "Point (1) 2" :: Point)
  print ("Point reads", reads "Point 1 2 rest" :: [(Point, String)], reads "Point 1" :: [(Point, String)], reads "point 1 2" :: [(Point, String)])
  print ("Point readsPrec 11", readsPrec 11 "Point 1 2" :: [(Point, String)], readsPrec 11 "(Point 1 2)" :: [(Point, String)])
  print ("Point in Maybe", read "Just (Point 1 2)" :: Maybe Point, null (reads "Just Point 1 2" :: [(Maybe Point, String)]))
  print ("Point in list (default readList)", read "[Point 1 2,Point 3 4]" :: [Point], read " [ ] " :: [Point], read "[(Point 0 0)]" :: [Point])
  print ("Point in tuple", read "(Point 1 2,True)" :: (Point, Bool), read "(Point 1 2,Point 3 4)" :: (Point, Point))
  print ("Point readList", readList "[Point 1 2, Point 3 4] tail" :: [([Point], String)])
  print ("Point round trip", map (\p -> read (show p) :: Point) [Point 0 0, Point (-5) 7], read (show (Just (Point (-1) (-1)))) :: Maybe Point)
  print ("Q", read "3 / 4" :: Q, read "(3 / 4)" :: Q, read "3/4" :: Q)
  print ("Q precedence", null (readsPrec 8 "3 / 4" :: [(Q, String)]), readsPrec 8 "(3 / 4)" :: [(Q, String)], readsPrec 7 "3 / 4" :: [(Q, String)])
  print ("Q in structures", read "[3 / 4,5 / 6]" :: [Q], read "Just (1 / 2)" :: Maybe Q, read "(1 / 2,3 / 4)" :: (Q, Q))
  print ("Q round trip", read (show (Q 1 2)) :: Q, read (show [Just (Q 1 2)]) :: [Maybe Q])
  print ("Switch", read "on" :: Switch, read " off " :: Switch, read "[on,off,on]" :: [Switch])
  print ("Switch reads", reads "on off" :: [(Switch, String)], reads "onoff" :: [(Switch, String)], reads "ON" :: [(Switch, String)])
  print ("Switch in structures", read "Just on" :: Maybe Switch, read "(on,off)" :: (Switch, Switch), read "Left off" :: Either Switch Int)
