-- | Making a whole program smaller before it is compiled for a machine,
-- by what can be seen without knowing any types.
--
-- What the lowering makes is plain: a method is a function that takes a
-- dictionary apart, an instance with no context is a dictionary written
-- out, and the library's fixed-size integer is the other one under a
-- coercion. So a sum of two @Int@s is four calls and a dictionary taken
-- apart. Here:
--
-- * A dictionary written out at the top has each of its fields made a
--   definition of its own, and a method applied to such a dictionary is
--   that definition.
-- * A coercion applied to something is the thing.
-- * A definition that is only another name for something is that thing,
--   at the top of a program and inside a definition.
-- * A small function that binds nothing but its parameters is written
--   out where it is given them all, if doing so does nothing twice.
--
-- None of it changes what a program means, or does work a program would
-- not have done. Names at the top of a program are never names inside a
-- definition, and what is written out binds nothing, so no name is
-- taken for another.
module Powder.Simplify (simplify, shaken) where

import Data.Bifunctor (second)
import Data.List (elemIndex, isPrefixOf, nub)
import qualified Data.Map.Strict as Map
import Powder.Core

-- | Only what one name reaches, made smaller: and then only what it
-- still reaches.
shaken :: Name -> Program -> Program
shaken root = shake root . simplify . shake root

simplify :: Program -> Program
simplify (Program cons defs exports) = Program cons (rounds (6 :: Int) (concatMap apart defs)) exports
  where
    rounds n ds
      | n <= 0 = ds
      | otherwise = rounds (n - 1) (again ds)

-- | A dictionary written out, with each field that is more than a name a
-- definition of its own.
apart :: (Name, Core) -> [(Name, Core)]
apart (x, e) = case spineOf e of
  (Con c, fields)
    | dictionary c,
      length fields == conArity c,
      not (all atomic fields) ->
        let named = [(x ++ "@" ++ show i, field) | (i, field) <- zip [0 :: Int ..] fields]
         in (x, foldl App (Con c) [if atomic field then field else Var n | (n, field) <- named]) : [(n, field) | (n, field) <- named, not (atomic field)]
  _ -> [(x, e)]

dictionary :: ConInfo -> Bool
dictionary c = "Dict#" `isPrefixOf` conName c

-- | Something that is no work to have twice.
atomic :: Core -> Bool
atomic = \case
  Var _ -> True
  Lit _ -> True
  Prim _ -> True
  Con _ -> True
  _ -> False

spineOf :: Core -> (Core, [Core])
spineOf = go []
  where
    go arguments = \case
      App f a -> go (a : arguments) f
      e -> (e, arguments)

-- | Every definition, with what the others are seen to be put to use.
again :: [(Name, Core)] -> [(Name, Core)]
again defs = map (second go) defs
  where
    env = Map.fromList defs
    -- A function that gives one field of a constructor: which, of which.
    selectors = Map.mapMaybe selector env
    selector = \case
      Lam d (Case (Var d') [Alt c xs (Var field)]) | d == d', Just i <- elemIndex field xs -> Just (conName c, i)
      _ -> Nothing
    -- A dictionary written out, its fields all names.
    dictionaries = Map.mapMaybe written env
    written e = case spineOf e of
      (Con c, fields) | dictionary c, length fields == conArity c, all atomic fields -> Just (conName c, fields)
      _ -> Nothing
    -- Another name for something.
    aliases = Map.filterWithKey (\x e -> atomic e && not (same x e)) env
    same x = \case
      Var y -> x == y
      _ -> False
    -- A small function that binds nothing but its parameters, and does
    -- not call itself.
    small = Map.mapMaybeWithKey function env
    function x e = case parameters e of
      (ps@(_ : _), body) | nub ps == ps, plain body, size body <= 24, not (mentions x body) -> Just (ps, body)
      _ -> Nothing
    parameters = \case
      Lam p body -> let (ps, inner) = parameters body in (p : ps, inner)
      body -> ([], body)

    go e = case e of
      Var x | Just what <- Map.lookup x aliases -> what
      App f a -> reduce (App (go f) (go a))
      Lam x body -> Lam x (go body)
      -- A name for what is only a name is that name, so long as nothing
      -- under it is called the same.
      Let (NonRec x bound) body -> case go bound of
        bound' | atomic bound', not (rebound bound' body) -> go (renamed x bound' body)
        bound' -> Let (NonRec x bound') (go body)
      Let (Rec bs) body -> Let (Rec (map (second go) bs)) (go body)
      Case scrutinee alts -> Case (go scrutinee) [Alt c xs (go body) | Alt c xs body <- alts]
      _ -> e

    reduce e = case e of
      App (Prim Coerce) a -> a
      App (Var s) (Var d)
        | Just (c, i) <- Map.lookup s selectors,
          Just (c', fields) <- Map.lookup d dictionaries,
          c == c',
          field : _ <- drop i fields ->
            field
      _ -> case spineOf e of
        (Var f, arguments)
          | Just (ps, body) <- Map.lookup f small,
            length arguments >= length ps,
            -- Nothing is done twice: what is not a name goes where the
            -- parameter is once.
            and [atomic a || count p body <= 1 | (p, a) <- zip ps arguments] ->
              foldl App (substitute (Map.fromList (zip ps arguments)) body) (drop (length ps) arguments)
        _ -> e

-- | Whether a name, if that is what something is, is bound anew
-- somewhere in another thing.
rebound :: Core -> Core -> Bool
rebound = \case
  Var y -> binds y
  _ -> const False
  where
    binds y = \case
      App f a -> binds y f || binds y a
      Lam x body -> x == y || binds y body
      Let (NonRec x bound) body -> x == y || binds y bound || binds y body
      Let (Rec bs) body -> any ((== y) . fst) bs || any (binds y . snd) bs || binds y body
      Case scrutinee alts -> binds y scrutinee || or [y `elem` xs || binds y body | Alt _ xs body <- alts]
      _ -> False

-- | Something with a thing in place of a name, wherever the name is not
-- bound anew.
renamed :: Name -> Core -> Core -> Core
renamed x what = go
  where
    go = \case
      Var y -> if y == x then what else Var y
      App f a -> App (go f) (go a)
      Lam y body -> Lam y (if y == x then body else go body)
      Let (NonRec y bound) body -> Let (NonRec y (go bound)) (if y == x then body else go body)
      Let (Rec bs) body
        | any ((== x) . fst) bs -> Let (Rec bs) body
        | otherwise -> Let (Rec [(y, go bound) | (y, bound) <- bs]) (go body)
      Case scrutinee alts -> Case (go scrutinee) [Alt c xs (if x `elem` xs then body else go body) | Alt c xs body <- alts]
      e -> e

-- | Whether something binds no names: it has no function in it, no
-- definition, and takes nothing apart that has fields.
plain :: Core -> Bool
plain = \case
  App f a -> plain f && plain a
  Case scrutinee alts -> plain scrutinee && and [null xs && plain body | Alt _ xs body <- alts]
  Lam _ _ -> False
  Let _ _ -> False
  Foreign {} -> False
  _ -> True

size :: Core -> Int
size = \case
  App f a -> 1 + size f + size a
  Case scrutinee alts -> 1 + size scrutinee + sum [size body | Alt _ _ body <- alts]
  _ -> 1

mentions :: Name -> Core -> Bool
mentions x e = count x e > 0

-- | How many times a name is in something that binds none.
count :: Name -> Core -> Int
count x = \case
  Var y -> if x == y then 1 else 0
  App f a -> count x f + count x a
  Case scrutinee alts -> count x scrutinee + sum [count x body | Alt _ _ body <- alts]
  _ -> 0

-- | Something that binds no names, with other things in place of some of
-- the names in it.
substitute :: Map.Map Name Core -> Core -> Core
substitute filling = go
  where
    go = \case
      Var y -> Map.findWithDefault (Var y) y filling
      App f a -> App (go f) (go a)
      Case scrutinee alts -> Case (go scrutinee) [Alt c xs (go body) | Alt c xs body <- alts]
      e -> e
