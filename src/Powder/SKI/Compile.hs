-- | Core to combinators by bracket abstraction, following MicroHs's
-- @MicroHs.Abstract@.
module Powder.SKI.Compile (compileProgram) where

import qualified Data.Set as Set
import Powder.Core
import Powder.SKI.Comb

-- | Compile every definition to a closed combinator expression. Definitions
-- refer to each other by name, so top-level recursion needs no 'Y'.
-- Constructors that are used become definitions too.
compileProgram :: Program -> [(Name, Comb)]
compileProgram (Program cons defs _) =
  [(conName con, improve (scott con)) | con <- cons, conName con `Set.member` used] ++ compiled
  where
    compiled = [(x, improve (compile e)) | (x, e) <- defs]
    used = Set.fromList (concatMap (refs . snd) compiled)

-- | Data is Scott-encoded: a constructor takes its fields and then one
-- continuation per constructor of its type, and passes the fields to its
-- own. So @x : xs@ is @\\nil cons -> cons x xs@.
scott :: ConInfo -> Comb
scott (ConInfo _ tag arity siblings) =
  foldr abstract (foldl (:@) (CVar (ks !! tag)) (map CVar xs)) (xs ++ ks)
  where
    xs = ["$x" ++ show i | i <- [1 .. arity]]
    ks = ["$k" ++ show i | i <- [1 .. siblings]]

compile :: Core -> Comb
compile = \case
  Var x -> CVar x
  Lit n -> CInt n
  Prim p -> CPrim p
  Foreign symbol arguments result -> CForeign symbol arguments result
  Con con -> CVar (conName con)
  App f a -> compile f :@ compile a
  Lam x e -> abstract x (compile e)
  Let (NonRec x e) body -> abstract x (compile body) :@ compile e
  Let (Rec [(x, e)]) body -> abstract x (compile body) :@ (CComb Y :@ abstract x (compile e))
  -- A mutually recursive group is the fixpoint of a tuple of its
  -- definitions:
  --   t = Y (\t -> (\x1 .. xn k -> k e1 .. en) (t sel1) .. (t seln))
  -- and each xi is the projection t seli.
  Let (Rec bs) body ->
    let xs = map fst bs
        bindAll e = apply (foldr abstract e xs) [CVar "$t" :@ foldr abstract (CVar x) xs | x <- xs]
        tuple = abstract "$k" (apply (CVar "$k") (map (compile . snd) bs))
     in abstract "$t" (bindAll (compile body)) :@ (CComb Y :@ abstract "$t" (bindAll tuple))
  -- With Scott encoding, case is application to the alternatives.
  Case scrut alts -> apply (compile scrut) [foldr abstract (compile e) xs | Alt _ xs e <- alts]
  where
    apply = foldl (:@)

-- | Bracket abstraction: @abstract x e@ is a term without @x@ that behaves
-- like @\\x -> e@.
--
-- The plain algorithm gives @S (\\x -> f) (\\x -> a)@ for an application
-- @f a@, @I@ for @x@ itself, and @K e@ for anything else. That is correct
-- but enormous, so 's' stands in for @S@ and picks something cheaper when
-- it can.
abstract :: Name -> Comb -> Comb
abstract x = \case
  CVar y | x == y -> CComb I
  f :@ a -> s (abstract x f) (abstract x a)
  e -> CComb K :@ e

-- | @s f a@, @b f a@ and @c f a@ behave like @S f a@, @B f a@ and @C f a@.
s, b, c :: Comb -> Comb -> Comb
s (CComb K) _ = CComb I
s (CComb K :@ f) (CComb K :@ a) = CComb K :@ (f :@ a)
s (CComb K :@ f) (CComb I) = f
s (CComb K :@ f) a = b f a
s f (CComb K :@ a) = c f a
s (CComb B :@ p :@ f) a = CComb S' :@ p :@ f :@ a
s f a = CComb S :@ f :@ a

b (CComb B :@ CComb K) (CComb P) = CComb O
b (CComb Y) (CComb B :@ CComb K :@ e) = e
b (CComb C) (CComb C :@ CComb I) = CComb P
b (CComb I) a = a
b f a = CComb B :@ f :@ a

c (CComb B :@ p :@ f) a = CComb C' :@ p :@ f :@ a
c (CComb C :@ CComb I :@ f) a = CComb P :@ f :@ a
c f a = CComb C :@ f :@ a

-- | A peephole pass over finished code, replacing common pairs of
-- combinators with single ones.
improve :: Comb -> Comb
improve = \case
  f :@ a -> case (improve f, improve a) of
    (CComb K, CComb I) -> CComb A
    (CComb B, CComb K) -> CComb Z
    (CComb C, CComb I) -> CComb U
    (CComb B, CComb B) -> CComb B'
    (CComb C, CComb C) -> CComb R
    (CComb C', CComb B) -> CComb C'B
    (CComb Z, CComb K) -> CComb K2
    (CComb Z, CComb K2) -> CComb K3
    (CComb Z, CComb K3) -> CComb K4
    (CComb Y, CComb K :@ e) -> e
    (CComb K, CComb K :@ e) -> CComb K2 :@ e
    (CComb K, CComb K2 :@ e) -> CComb K3 :@ e
    (CComb K, CComb K3 :@ e) -> CComb K4 :@ e
    (f', a') -> f' :@ a'
  e -> e
