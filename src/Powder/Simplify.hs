-- | Making a whole program smaller before it is compiled for a machine,
-- by what can be seen without knowing any types.
--
-- What the lowering makes is plain: a method is a function that takes a
-- dictionary apart, an instance with no context is a dictionary written
-- out, and the library's fixed-size integer is the other one under a
-- coercion. So a sum of two @Int@s is calls and a dictionary taken apart.
--
-- Over the whole program:
--
-- * A dictionary written out at the top has each of its fields made a
--   definition of its own.
-- * An instance that needs others is a function of their dictionaries,
--   and makes its dictionary again each time it is asked for. Where it is
--   applied to dictionaries that are known, it is made once for those, as
--   a definition of its own that is a dictionary written out.
--
-- And in each definition, a few times over:
--
-- * A value bound and used once, not inside a function, is put where it
--   is used; one used once inside a function is put there too if it is a
--   function or a constructor, which is no work to make; one not used is
--   gone; and a name for only a name, or a number, is that.
-- * A function applied is its body, with its parameter bound to the
--   argument, as a @let@: so the argument is still worked out at most
--   once.
-- * A small function defined at the top is written out where it is given
--   all its arguments, its names made new.
-- * A case of a constructor it can see is the clause for that
--   constructor: of one built there, of a name bound to one, of a name
--   inside a clause that took it apart, or of a dictionary at the top.
-- * Arithmetic on numbers written out is done, and a coercion applied to
--   something is the thing.
--
-- None of it changes what a program means, or does work a program would
-- not have done. Every name a definition binds is made new before it is
-- made smaller, and so is every name in what is copied into it, so each
-- is bound once: a name put in place of another is never taken for a
-- third.
module Powder.Simplify (simplify, shaken) where

import Control.Monad (forM)
import Control.Monad.State.Strict
import Data.Bifunctor (second)
import Data.Int (Int64)
import Data.List (intercalate, isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Powder.Core
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

-- | Only what one name reaches, made smaller: and then only what it
-- still reaches.
shaken :: Name -> Program -> Program
shaken root = shake root . (if off then id else simplify) . shake root

-- | Whether POWDER_SIMPLIFY=0 says to leave a program as it is, to see
-- what the lowering made, or whether a fault is this module's.
off :: Bool
off = unsafePerformIO (lookupEnv "POWDER_SIMPLIFY") == Just "0"
{-# NOINLINE off #-}

simplify :: Program -> Program
simplify (Program cons defs exports) = Program cons (evalState (rounds (4 :: Int) (concatMap apart defs)) 0) exports
  where
    rounds n ds
      | n <= 0 = pure ds
      | otherwise = again ds >>= rounds (n - 1)

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
  Con c -> conArity c == 0
  _ -> False

spineOf :: Core -> (Core, [Core])
spineOf = go []
  where
    go arguments = \case
      App f a -> go (a : arguments) f
      e -> (e, arguments)

parameters :: Core -> ([Name], Core)
parameters = \case
  Lam p body -> let (ps, inner) = parameters body in (p : ps, inner)
  body -> ([], body)

-- | What a name made new is called: a number no other has after it.
type M = State Int

newName :: Name -> M Name
newName x = state (\n -> (takeWhile (/= '~') x ++ "~" ++ show n, n + 1))

-- The program -----------------------------------------------------------------

-- | Every definition made smaller, with what the others are seen to be.
again :: [(Name, Core)] -> M [(Name, Core)]
again defs = do
  let env = Map.fromList defs
      -- An instance that needs others: a function of their dictionaries.
      needing = Map.mapMaybeWithKey (\x e -> case parameters e of (ps@(_ : _), body) | instance' x -> Just (ps, body); _ -> Nothing) env
      instance' = ("inst#" `isPrefixOf`)
      known = \case
        Var g | instance' g, Map.member g env -> Just g
        _ -> Nothing
      -- Such a one applied to dictionaries that are known: which, to
      -- which, and what the result is applied to besides.
      site e = case spineOf e of
        (Var f, arguments)
          | Just (ps, _) <- Map.lookup f needing,
            length arguments >= length ps,
            Just given <- mapM known (take (length ps) arguments) ->
              Just (f, given, drop (length ps) arguments)
        _ -> Nothing
      special f given = f ++ "<" ++ intercalate "," given ++ ">"
      sites e =
        [(f, given) | Just (f, given, _) <- [site e]] ++ case e of
          App f a -> sites f ++ sites a
          Lam _ body -> sites body
          Let (NonRec _ bound) body -> sites bound ++ sites body
          Let (Rec bs) body -> concatMap (sites . snd) bs ++ sites body
          Case scrutinee alts -> sites scrutinee ++ concat [sites body | Alt _ _ body <- alts]
          _ -> []
  -- Each made once for the dictionaries it is given somewhere, its
  -- names made new.
  made <-
    fmap Map.fromList . sequence $
      [ (,) name <$> fresh Map.empty (foldr (\(p, g) -> Let (NonRec p (Var g))) body (zip ps given))
        | (name, (f, given)) <- Map.toList (Map.fromList [(special f given, (f, given)) | (_, e) <- defs, (f, given) <- sites e]),
          Map.notMember name env,
          Just (ps, body) <- [Map.lookup f needing]
      ]
  let -- A site is the one made for it, once it has been made.
      specialised e = case site e of
        Just (f, given, rest)
          | Map.member (special f given) env || Map.member (special f given) made -> foldl App (Var (special f given)) (map specialised rest)
        _ -> case e of
          App f a -> App (specialised f) (specialised a)
          Lam x body -> Lam x (specialised body)
          Let (NonRec x bound) body -> Let (NonRec x (specialised bound)) (specialised body)
          Let (Rec bs) body -> Let (Rec (map (second specialised) bs)) (specialised body)
          Case scrutinee alts -> Case (specialised scrutinee) [Alt c xs (specialised body) | Alt c xs body <- alts]
          other -> other
      everything = defs ++ concatMap apart (Map.toList made)
      tops = tops' (Map.fromList everything)
  concatMap apart <$> forM everything (\(x, e) -> (,) x <$> definition tops (specialised e))

-- | What is known of the definitions at the top.
data Tops = Tops
  { -- | Those that are only another name for something.
    topAliases :: Map.Map Name Core,
    -- | Those that are a constructor applied to names: dictionaries.
    topConstructed :: Map.Map Name (ConInfo, [Core]),
    -- | Small functions that do not call themselves: their parameters and
    -- body.
    topSmall :: Map.Map Name ([Name], Core)
  }

tops' :: Map.Map Name Core -> Tops
tops' env = Tops aliases constructed small
  where
    aliases = Map.filterWithKey (\x e -> atomic e && not (same x e)) env
    same x = \case
      Var y -> x == y
      _ -> False
    constructed = Map.mapMaybe written env
    written e = case spineOf e of
      (Con c, fields) | length fields == conArity c, conArity c > 0, all atomic fields -> Just (c, fields)
      _ -> Nothing
    small = Map.mapMaybeWithKey function env
    function x e = case parameters e of
      (ps@(_ : _), body) | size body <= 30, not (mentions x body) -> Just (ps, body)
      _ -> Nothing

-- A definition -------------------------------------------------------------------

-- | What a name that was bound is to be, where it is used.
data Sub
  = -- | Something already made smaller.
    Done Core
  | -- | Something to make smaller where it is used, with what was known
    -- where it was bound. It is used once.
    Later Env Core

-- | What is known, where something is made smaller.
data Env = Env
  { subst :: Map.Map Name Sub,
    -- | Names known to be a constructor applied to some things.
    constructed :: Map.Map Name (ConInfo, [Core]),
    -- | How many times each name the definition binds is used, and where.
    occurring :: Map.Map Name Occ,
    tops :: Tops,
    -- | How many functions from the top are being written out, one in
    -- another: so that two that call each other are not forever.
    depth :: Int
  }

-- | How a name is used where it is bound: never, once (inside a function
-- made there or not), or more.
data Occ = Dead | Once Bool | Many
  deriving (Eq)

-- | A definition made smaller. Its names are made new first: the
-- lowering may bind one name in two places, one inside the other.
definition :: Tops -> Core -> M Core
definition ts e = fresh Map.empty e >>= go (2 :: Int)
  where
    go 0 e = pure e
    go n e = simplified (Env Map.empty Map.empty (occurrences e) ts 0) e >>= go (n - 1)

-- | How each name an expression binds is used in what it is bound for.
occurrences :: Core -> Map.Map Name Occ
occurrences = snd . go
  where
    -- The names used and not bound, and how; and how each bound is used.
    go :: Core -> (Map.Map Name Occ, Map.Map Name Occ)
    go = \case
      Var x -> (Map.singleton x (Once False), Map.empty)
      App f a -> go f <+> go a
      Lam x body -> let (free, bound) = go body in (Map.map inside (Map.delete x free), Map.insert x (use x free) bound)
      Let (NonRec x e) body ->
        let (free, bound) = go body
         in go e <+> (Map.delete x free, Map.insert x (use x free) bound)
      -- What refers to itself may be worked out over and over.
      Let (Rec bs) body ->
        let xs = map fst bs
            (free, bound) = foldr ((<+>) . first' (Map.map inside) . go . snd) (go body) bs
         in (foldr Map.delete free xs, foldr (\x -> Map.insert x (if Map.member x free then Many else Dead)) bound xs)
      -- A name in two clauses would be written out twice.
      Case scrutinee alts -> foldr ((<+>) . clause) (go scrutinee) alts
      _ -> (Map.empty, Map.empty)
    clause (Alt _ xs body) = let (free, bound) = go body in (foldr Map.delete free xs, foldr (\x -> Map.insert x (use x free)) bound xs)
    use x free = fromMaybe Dead (Map.lookup x free)
    inside = \case
      Once _ -> Once True
      o -> o
    (a, b) <+> (c, d) = (Map.unionWith (\_ _ -> Many) a c, Map.union b d)
    first' f (a, b) = (f a, b)

occurrence :: Env -> Name -> Occ
occurrence env x = fromMaybe Many (Map.lookup x (occurring env))

-- | Whether something is no work to make: a function, or a constructor
-- of things that are no work.
value :: Core -> Bool
value e = case spineOf e of
  (Lam _ _, []) -> True
  (Con c, fields) -> length fields <= conArity c && all atomic fields
  (f, []) -> atomic f
  _ -> False

simplified :: Env -> Core -> M Core
simplified env = \case
  Var x -> case Map.lookup x (subst env) of
    Just (Done e) -> pure e
    Just (Later env' e) -> simplified env' e
    Nothing -> pure (fromMaybe (Var x) (Map.lookup x (topAliases (tops env))))
  e@(App _ _) -> applied env e
  Lam x body -> Lam x <$> simplified env body
  Let (NonRec x bound) body -> bind env env x bound body
  Let (Rec bs) body -> do
    bs' <- forM bs (\(x, bound) -> (,) x <$> simplified env bound)
    body' <- simplified env body
    -- Gone, if nothing outside them refers to them.
    pure (if any (\(x, _) -> mentions x body') bs' then Let (Rec bs') body' else body')
  Case scrutinee alts -> do
    scrutinee' <- simplified env scrutinee
    cased env scrutinee' alts
  e -> pure e

-- | A case of something already made smaller, with clauses not yet.
cased :: Env -> Core -> [Alt] -> M Core
cased env scrutinee alts = case constructedAs env scrutinee of
  Just (c, fields)
    | Alt _ xs body : _ <- drop (conTag c) alts,
      length xs == length fields ->
        -- The fields are made smaller already: what is bound to them is
        -- done, or a let of it.
        bindDone env (zip xs fields) body
  _ -> case scrutinee of
    -- A case of a case with one clause: the outer one in its clause.
    Case inner [Alt c xs body] -> do
      body' <- cased env body alts
      pure (Case inner [Alt c xs body'])
    _ -> Case scrutinee <$> forM alts (\(Alt c xs body) -> Alt c xs <$> simplified (inClause scrutinee c xs env) body)

-- | A name bound to something, for what it is bound for: the something
-- with what was known where it was, the rest with what is known here.
bind :: Env -> Env -> Name -> Core -> Core -> M Core
bind from env x bound body = case occurrence env x of
  Dead -> simplified env body
  Once False -> simplified (env {subst = Map.insert x (Later from bound) (subst env)}) body
  used -> do
    bound' <- simplified from bound
    if atomic bound' || (used == Once True && value bound')
      then simplified (env {subst = Map.insert x (Done bound') (subst env)}) body
      else Let (NonRec x bound') <$> simplified (knowing x bound' env) body

-- | Names bound to things already made smaller.
bindDone :: Env -> [(Name, Core)] -> Core -> M Core
bindDone env [] body = simplified env body
bindDone env ((x, bound) : rest) body = case occurrence env x of
  Dead -> bindDone env rest body
  used
    | atomic bound || used == Once False || (used == Once True && value bound) ->
        bindDone (env {subst = Map.insert x (Done bound) (subst env)}) rest body
    | otherwise -> Let (NonRec x bound) <$> bindDone (knowing x bound env) rest body

-- | In a clause of a case of a name, the name is known to be that
-- constructor of the names the clause gives its fields.
inClause :: Core -> ConInfo -> [Name] -> Env -> Env
inClause scrutinee c xs env = case scrutinee of
  Var v -> env {constructed = Map.insert v (c, map Var xs) (constructed env)}
  _ -> env

knowing :: Name -> Core -> Env -> Env
knowing x bound env = case spineOf bound of
  (Con c, fields) | length fields == conArity c, all atomic fields -> env {constructed = Map.insert x (c, fields) (constructed env)}
  _ -> env

-- | The constructor something is, and its fields, if that can be seen.
constructedAs :: Env -> Core -> Maybe (ConInfo, [Core])
constructedAs env e = case spineOf e of
  (Con c, fields) | length fields == conArity c -> Just (c, fields)
  (Var v, []) -> case Map.lookup v (constructed env) of
    Just known -> Just known
    Nothing -> Map.lookup v (topConstructed (tops env))
  _ -> Nothing

-- | Something applied to arguments, none of it made smaller yet.
applied :: Env -> Core -> M Core
applied env e = case spineOf e of
  -- A function applied: its body, its parameter bound to the argument.
  (Lam x body, a : rest) -> bind env env x a (foldl App body rest)
  (Prim Coerce, a : rest) -> simplified env (foldl App a rest)
  -- A small function from the top, written out with its names new. Its
  -- arguments are bound with what is known here.
  (Var g, arguments)
    | Map.notMember g (subst env),
      depth env < 4,
      Just (ps, body) <- Map.lookup g (topSmall (tops env)),
      length arguments >= length ps -> do
        copy <- fresh Map.empty (foldr Lam body ps)
        let (ps', body') = parameters copy
            inner = env {depth = depth env + 1, occurring = Map.union (occurrences copy) (occurring env)}
        bindAll env inner (zip ps' arguments) (foldl App body' (drop (length ps) arguments))
  (f, arguments) -> do
    f' <- simplified env f
    arguments' <- mapM (simplified env) arguments
    case (f', arguments') of
      -- A function that is known only now: bound to what is made already.
      (Lam x body, a : rest) -> do
        let env' = env {subst = Map.empty, occurring = Map.union (occurrences f') (occurring env)}
        bindDone env' [(x, a)] (foldl App body rest)
      (Prim p, [Lit a, Lit b]) | Just n <- folded p a b -> pure n
      (Prim IntNarrow, [Lit a]) -> pure (Lit (toInteger (fromInteger a :: Int64)))
      _ -> pure (foldl App f' arguments')

bindAll :: Env -> Env -> [(Name, Core)] -> Core -> M Core
bindAll _ env [] body = simplified env body
bindAll from env ((x, a) : rest) body = case occurrence env x of
  Dead -> bindAll from env rest body
  Once False -> bindAll from (env {subst = Map.insert x (Later from a) (subst env)}) rest body
  used -> do
    a' <- simplified from a
    if atomic a' || (used == Once True && value a')
      then bindAll from (env {subst = Map.insert x (Done a') (subst env)}) rest body
      else Let (NonRec x a') <$> bindAll from (knowing x a' env) rest body

-- | Arithmetic on two numbers written out, done.
folded :: Prim -> Integer -> Integer -> Maybe Core
folded p a b = case p of
  IntAdd -> Just (Lit (a + b))
  IntSub -> Just (Lit (a - b))
  IntMul -> Just (Lit (a * b))
  IntEq -> Just (truth (a == b))
  IntLt -> Just (truth (a < b))
  IntLe -> Just (truth (a <= b))
  _ -> Nothing
  where
    truth t = Con (if t then trueCon else falseCon)

-- | Something with every name it binds made new, and names given in place
-- of some it does not.
fresh :: Map.Map Name Name -> Core -> M Core
fresh names = \case
  Var x -> pure (Var (Map.findWithDefault x x names))
  App f a -> App <$> fresh names f <*> fresh names a
  Lam x body -> do
    x' <- newName x
    Lam x' <$> fresh (Map.insert x x' names) body
  Let (NonRec x bound) body -> do
    x' <- newName x
    Let . NonRec x' <$> fresh names bound <*> fresh (Map.insert x x' names) body
  Let (Rec bs) body -> do
    xs' <- mapM (newName . fst) bs
    let names' = Map.union (Map.fromList (zip (map fst bs) xs')) names
    bs' <- forM (zip xs' bs) (\(x', (_, bound)) -> (,) x' <$> fresh names' bound)
    Let (Rec bs') <$> fresh names' body
  Case scrutinee alts -> do
    scrutinee' <- fresh names scrutinee
    Case scrutinee' <$> forM alts (\(Alt c xs body) -> do
      xs' <- mapM newName xs
      Alt c xs' <$> fresh (Map.union (Map.fromList (zip xs xs')) names) body)
  e -> pure e

size :: Core -> Int
size = \case
  App f a -> 1 + size f + size a
  Lam _ body -> 1 + size body
  Let (NonRec _ bound) body -> 1 + size bound + size body
  Let (Rec bs) body -> 1 + sum (map (size . snd) bs) + size body
  Case scrutinee alts -> 1 + size scrutinee + sum [size body | Alt _ _ body <- alts]
  _ -> 1

mentions :: Name -> Core -> Bool
mentions x = \case
  Var y -> x == y
  App f a -> mentions x f || mentions x a
  Lam _ body -> mentions x body
  Let (NonRec _ bound) body -> mentions x bound || mentions x body
  Let (Rec bs) body -> any (mentions x . snd) bs || mentions x body
  Case scrutinee alts -> mentions x scrutinee || or [mentions x body | Alt _ _ body <- alts]
  _ -> False
