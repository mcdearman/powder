-- Haskell 2010 Report 10.6 / 4.4.2: resolution with user fixities at every level 0-9, all three associativities
module Main (main) where

infixl 0 <@
infixr 1 @>
infix  2 <?>
infixr 3 &&&
infixl 4 +?
infixr 5 +:
infixl 6 .+.
infixl 7 .*.
infixr 8 .^.
infixl 9 .!.

data E = L String | B String E E

instance Show E where
  show (L s) = s
  show (B o a b) = "(" ++ show a ++ o ++ show b ++ ")"

(<@), (@>), (<?>), (&&&), (+?), (+:), (.+.), (.*.), (.^.), (.!.), (%%) :: E -> E -> E
(<@) = B "<@"
(@>) = B "@>"
(<?>) = B "<?>"
(&&&) = B "&&&"
(+?) = B "+?"
(+:) = B "+:"
(.+.) = B ".+."
(.*.) = B ".*."
(.^.) = B ".^."
(.!.) = B ".!."
(%%) = B "%%"

op :: E -> E -> E
op = B "`op`"

a, b, c, d :: E
a = L "a"
b = L "b"
c = L "c"
d = L "d"

main :: IO ()
main = do
  print ("l0", a <@ b <@ c)
  print ("r1", a @> b @> c)
  print ("n2 with tighter", a .+. b <?> c .+. d)
  print ("r3", a &&& b &&& c)
  print ("l4", a +? b +? c)
  print ("r5", a +: b +: c)
  print ("l6", a .+. b .+. c)
  print ("l7", a .*. b .*. c)
  print ("r8", a .^. b .^. c)
  print ("l9", a .!. b .!. c)
  print ("default fixity infixl 9 symbol", a %% b %% c, a %% b .^. c)
  print ("default fixity infixl 9 backquote", a `op` b `op` c, a .!. b `op` c)
  print ("ladder up", a <@ b @> c <?> d)
  print ("ladder 6 7 8", a .+. b .*. c .^. d)
  print ("ladder 8 7 6", a .^. b .*. c .+. d)
  print ("mixed", a .+. b .*. c .+. d)
  print ("mixed2", a .*. b .+. c .*. d)
  print ("r5 over l4", a +: b +? c +: d)
  print ("r3 under r5", a +: b &&& c +: d)
  print ("zero level split", a .+. b <@ c .*. d <@ a)
  print ("r1 and l0", a @> b <@ c @> d)
  print ("9 vs application", a .!. op b c)
  print ("with parens", (a .+. b) .*. (c .+. d))
  print ("section left", (a .^. b .*.) c)
  print ("section right", (.*. a .^. b) c)
  print ("section of tighter left operand", (a .*. b .+.) c)
  print ("section same-level left assoc", (a .+. b .+.) c)
  print ("section same-level right assoc", (.^. a .^. b) c)
  print ("backquote section", (`op` a) b, (a `op`) b)
