-- Haskell 2010 Report 2.4: identifiers: primes, underscores, digits, case distinction, names resembling keywords
module Main (main) where

data T' = C' Int | C_1 Int | Cx'y Int deriving Show

x' :: Int
x' = 1

x'' :: Int
x'' = 2

_y :: Int
_y = 3

_y' :: Int
_y' = 4

a_b_c :: Int
a_b_c = 5

a1b2 :: Int
a1b2 = 6

camelCase, camelcase :: Int
camelCase = 7
camelcase = 8

-- identifiers that merely begin with or contain reserved words
ifx, thenx, elsex, letx, inx, ofx, wherex, dox, casex, datax, classx, modulex, importx, typex :: Int
ifx = 10
thenx = 11
elsex = 12
letx = 13
inx = 14
ofx = 15
wherex = 16
dox = 17
casex = 18
datax = 19
classx = 20
modulex = 21
importx = 22
typex = 23

-- special identifiers that are not reserved: as, qualified, hiding
as, qualified, hiding :: Int
as = 30
qualified = 31
hiding = 32

-- other words that are not reserved in Haskell 2010
forall, family, role, pattern, mdo, proc, rec, group, by, using, static :: Int
forall = 40
family = 41
role = 42
pattern = 43
mdo = 44
proc = 45
rec = 46
group = 47
by = 48
using = 49
static = 50

f' :: Int -> Int
f' n' = n' + 1

main :: IO ()
main = do
  print ("primes", x', x'', f' 1)
  print ("underscore", _y, _y', a_b_c, a1b2)
  print ("case sensitive", camelCase, camelcase)
  print ("kw prefix 1", ifx, thenx, elsex, letx, inx)
  print ("kw prefix 2", ofx, wherex, dox, casex, datax)
  print ("kw prefix 3", classx, modulex, importx, typex)
  print ("special ids", as, qualified, hiding)
  print ("non-reserved 1", forall, family, role, pattern, mdo, proc)
  print ("non-reserved 2", rec, group, by, using, static)
  print ("constructors", C' 1, C_1 2, Cx'y 3)
  print ("underscore-bound lambda", (\_a -> _a + 1) (1 :: Int))
  print ("wildcards", (\_ _ -> 'k') 'a' 'b')
