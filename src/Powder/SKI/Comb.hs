-- | Combinator expressions.
module Powder.SKI.Comb
  ( Combinator (..),
    Comb (..),
    pretty,
    size,
    refs,
  )
where

import Powder.Core (Name, Prim, primName)

-- | The combinators MicroHs compiles to, with their reduction rules:
--
-- > S x y z       = x z (y z)
-- > K x y         = x
-- > I x           = x
-- > B x y z       = x (y z)
-- > C x y z       = x z y
-- > S' x y z w    = x (y w) (z w)
-- > B' x y z w    = x y (z w)
-- > C' x y z w    = x (y w) z
-- > A x y         = y
-- > U x y         = y x
-- > Y x           = x (Y x)
-- > Z x y z       = x y
-- > P x y z       = z x y
-- > R x y z       = y z x
-- > O x y z w     = w x y
-- > K2 x y z      = x
-- > K3 x y z w    = x
-- > K4 x y z w v  = x
-- > C'B x y z w   = x z (y w)
data Combinator = S | K | I | B | C | S' | B' | C' | A | U | Y | Z | P | R | O | K2 | K3 | K4 | C'B
  deriving (Eq, Show)

data Comb
  = -- | A variable being abstracted or, in finished code, a global.
    CVar Name
  | CInt Integer
  | CPrim Prim
  | -- | A C function, as in Core.
    CForeign Name String Char
  | CComb Combinator
  | -- | Application.
    Comb :@ Comb

infixl 9 :@

pretty :: Comb -> String
pretty e = go False e ""
  where
    go p = \case
      CVar x -> showString x
      CInt n -> showChar '#' . shows n
      CPrim q -> showString (primName q)
      CForeign symbol _ _ -> showString ("foreign:" ++ symbol)
      CComb c -> shows c
      f :@ a -> showParen p (go False f . showChar ' ' . go True a)

-- | The number of leaves.
size :: Comb -> Int
size = \case
  f :@ a -> size f + size a
  _ -> 1

refs :: Comb -> [Name]
refs = \case
  CVar x -> [x]
  f :@ a -> refs f ++ refs a
  _ -> []
