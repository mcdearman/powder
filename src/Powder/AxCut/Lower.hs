-- | The sequent calculus to AxCut.
--
-- Two things are done, in turn. /Focusing/ gives a name to everything a
-- statement uses that is not one already, so that each cut has a variable
-- on one side: a constructor's fields, a function's argument and a
-- primitive's become variables, and a consumer that is passed becomes a
-- covariable. Then /linearising/ puts the environment in order: before
-- each statement the row of values is arranged to be exactly what the
-- statement takes followed by what is still wanted after it, which is
-- worked out from what the rest of the program mentions.
--
-- A consumer that has to be passed is made an object, of the one method
-- that takes a value. That is the only place a continuation is ever
-- allocated.
module Powder.AxCut.Lower (lower) where

import Control.Monad
import Control.Monad.State
import qualified Data.Set as Set
import qualified Powder.AxCut as AxCut
import Powder.Sequent

type M = State Int

fresh :: String -> M Name
fresh hint = state (\n -> (hint ++ "~" ++ show n, n + 1))

lower :: Program -> AxCut.Program
lower (Program defs entry (given, callback) exports) = evalState build 0
  where
    build = do
      definitions <- forM defs $ \(x, p) -> case p of
        Thunk a s -> (,,) x a <$> (focus s >>= linear [a])
        _ -> error ("Powder.AxCut.Lower: the definition of " ++ x ++ " is not a thunk")
      AxCut.Program definitions <$> (focus entry >>= linear []) <*> ((,) given <$> (focus callback >>= linear [given])) <*> pure exports

-- Focusing ------------------------------------------------------------------

-- | Whether a producer is a name already.
named :: Producer -> Bool
named = \case
  Var _ -> True
  _ -> False

-- | Give names to what a statement uses, so that it is one of the forms
-- 'linear' takes.
focus :: Statement -> M Statement
focus = \case
  Halt -> pure Halt
  LetRec bs s -> LetRec <$> mapM (\(x, p) -> (,) x <$> inside p) bs <*> focus s
  Prim p arguments k -> naming arguments $ \xs -> do
    v <- fresh "v"
    Prim p xs . MuTilde v <$> focus (Cut (Var v) k)
  Foreign symbol shape result arguments k -> naming arguments $ \xs -> do
    v <- fresh "v"
    Foreign symbol shape result xs . MuTilde v <$> focus (Cut (Var v) k)
  Cut p c -> case (p, c) of
    -- A consumer named: nothing is made, the names are the same thing.
    (Mu a s, Covar _) -> (\s' -> Cut (Mu a s') c) <$> focus s
    (Mu a s, _) -> Cut . Mu a <$> focus s <*> consumer c
    (Var _, Covar _) -> pure (Cut p c)
    (Var _, MuTilde x s) -> Cut p . MuTilde x <$> focus s
    (Var _, Case alts) -> Cut p . Case <$> mapM (\(Alt k xs s) -> Alt k xs <$> focus s) alts
    (Var _, Ap argument k) -> naming [argument] $ \xs -> passing k (pure . Cut p . Ap (head' xs))
    (Var _, Force k) -> passing k (pure . Cut p . Force)
    (Var _, Run ok err) -> passing ok (\ok' -> passing err (pure . Cut p . Run ok'))
    -- Something made is named, and then used by its name.
    (_, MuTilde x s) -> focus s >>= bound p x
    _ -> do
      x <- fresh "x"
      focus (Cut p (MuTilde x (Cut (Var x) c)))
  where
    head' = \case
      x : _ -> x
      [] -> error "Powder.AxCut.Lower: nothing was named"

-- | Focus what is inside something made.
inside :: Producer -> M Producer
inside = \case
  Fun x a s -> Fun x a <$> focus s
  Thunk a s -> Thunk a <$> focus s
  Action a b s -> Action a b <$> focus s
  -- Only where things refer to each other is a constructor's field left
  -- to be anything but a name, and there it must be one already.
  p -> pure p

-- | A consumer that stands where it is, focused inside.
consumer :: Consumer -> M Consumer
consumer = \case
  Covar a -> pure (Covar a)
  MuTilde x s -> MuTilde x <$> focus s
  c -> do
    x <- fresh "x"
    MuTilde x <$> focus (Cut (Var x) c)

-- | Name producers, in order, and go on with them as variables.
naming :: [Producer] -> ([Producer] -> M Statement) -> M Statement
naming [] inner = inner []
naming (p : ps) inner
  | named p = naming ps (inner . (p :))
  | otherwise = do
      x <- fresh "x"
      naming ps (inner . (Var x :)) >>= bound p x

-- | Something made, given a name, and then a statement that is focused
-- already.
bound :: Producer -> Name -> Statement -> M Statement
bound p x rest = case p of
  Con k fields -> naming fields (\xs -> pure (Cut (Con k xs) (MuTilde x rest)))
  Mu a s -> (\s' -> Cut (Mu a s') (MuTilde x rest)) <$> focus s
  _ -> (\p' -> Cut p' (MuTilde x rest)) <$> inside p

-- | Pass a consumer: if it is not a name, give it one.
passing :: Consumer -> (Consumer -> M Statement) -> M Statement
passing k inner = case k of
  Covar _ -> inner k
  _ -> do
    a <- fresh "k"
    Cut . Mu a <$> inner (Covar a) <*> consumer k

-- Linearising ---------------------------------------------------------------

-- | The environment: the names of its values, the last last.
type Env = [Name]

-- | A statement linearised: the names it uses and does not bind, and what
-- it is in AxCut given the environment it is to run in. The first is
-- worked out once, from the inside out, and is what says at each
-- statement which values are still wanted.
type Linear = (Set.Set Name, Env -> AxCut.Statement)

-- | Arrange the environment to be some values, each named with the one it
-- is now, and go on. Nothing is said if it is that already.
arrange :: Env -> [(Name, Name)] -> AxCut.Statement -> AxCut.Statement
arrange env wanted s
  | map fst wanted == env && all (uncurry (==)) wanted = s
  | otherwise = AxCut.Substitute wanted s

-- | The values of an environment that are wanted, in the order they are in.
live :: Env -> Set.Set Name -> Env
live env wanted = filter (`Set.member` wanted) env

same :: [Name] -> [(Name, Name)]
same = map (\x -> (x, x))

-- | A statement that takes some values off the end of the environment
-- and goes nowhere else.
leaving :: [Name] -> AxCut.Statement -> Linear
leaving taken s = (Set.fromList taken, \env -> arrange env (same taken) s)

-- | Something that takes some values off the end, puts one there under a
-- name, and goes on.
binding :: [Name] -> Name -> (AxCut.Statement -> AxCut.Statement) -> Linear -> Linear
binding taken y make (wanted, rest) =
  ( Set.fromList taken <> after,
    \env -> let kept = live env after in arrange env (same (kept ++ taken)) (make (rest (kept ++ [y])))
  )
  where
    after = Set.delete y wanted

linear :: Env -> Statement -> M AxCut.Statement
linear env s = (\(_, make) -> make env) <$> linearly s

linearly :: Statement -> M Linear
linearly = \case
  Halt -> pure (leaving [] AxCut.Halt)
  -- A value to its consumer.
  Cut (Var x) (Covar a) -> pure (leaving [x, a] (AxCut.Invoke AxCut.Ret))
  -- A name for a value: the same value under the new name.
  Cut (Var x) (MuTilde y s) -> renamed y x <$> linearly s
  Cut (Var x) (Case alts) -> do
    clauses <- forM alts (\(Alt k xs s) -> (,,) k xs <$> linearly s)
    let wanted = mconcat [free Set.\\ Set.fromList xs | (_, xs, (free, _)) <- clauses]
    pure
      ( Set.insert x wanted,
        \env ->
          let kept = live env wanted
           in arrange env (same (kept ++ [x])) (AxCut.Switch [AxCut.Clause k xs (make (kept ++ xs)) | (k, xs, (_, make)) <- clauses])
      )
  Cut (Var f) (Ap (Var x) (Covar a)) -> pure (leaving [x, a, f] (AxCut.Invoke AxCut.Ap))
  Cut (Var x) (Force (Covar a)) -> pure (leaving [a, x] AxCut.Force)
  Cut (Var m) (Run (Covar ok) (Covar err)) -> pure (leaving [ok, err, m] (AxCut.Invoke AxCut.Run))
  -- Something made, and named.
  Cut made (MuTilde y s) | not (named made) -> do
    rest <- linearly s
    case made of
      Lit n -> pure (binding [] y (AxCut.Lit y n) rest)
      Global g -> pure (binding [] y (AxCut.Global y g) rest)
      Con k fields -> pure (binding (map variable fields) y (AxCut.Let y k) rest)
      Mu a body -> do
        -- What consumes it is the rest: an object that takes the value.
        (wantedIn, inner) <- linearly body
        let (wantedAfter, continuation) = rest
            capturedSet = Set.delete y wantedAfter
            before = Set.delete a wantedIn
        pure
          ( before <> capturedSet,
            \env ->
              let captured = live env capturedSet
                  kept = live env before
                  object = AxCut.Object AxCut.Closure captured (AxCut.Method AxCut.Ret [y] (continuation (y : captured)))
               in arrange env (same (kept ++ captured)) (AxCut.New a object (inner (kept ++ [a])))
          )
      _ -> do
        (capturedSet, object) <- objectOf made
        let (wantedAfter, continue) = rest
            after = Set.delete y wantedAfter
        pure
          ( capturedSet <> after,
            \env ->
              let captured = live env capturedSet
                  kept = live env after
               in arrange env (same (kept ++ captured)) (AxCut.New y (object captured) (continue (kept ++ [y])))
          )
  -- A consumer named by another name.
  Cut (Mu a s) (Covar b) -> renamed a b <$> linearly s
  Prim p arguments (MuTilde v s) -> binding (map variable arguments) v (AxCut.Prim v p (length arguments)) <$> linearly s
  Foreign symbol shape result arguments (MuTilde v s) -> binding (map variable arguments) v (AxCut.Foreign v symbol shape result) <$> linearly s
  -- Each captures what it mentions of the environment, and then those of
  -- the others it mentions.
  LetRec bs s -> do
    let names = map fst bs
        nameSet = Set.fromList names
    objects <- forM bs (\(x, p) -> (,) x <$> objectOf p)
    (wantedAfter, continue) <- linearly s
    let after = wantedAfter Set.\\ nameSet
        outside = mconcat [free Set.\\ nameSet | (_, (free, _)) <- objects]
    pure
      ( after <> outside,
        \env ->
          let kept = live env after
              built =
                [ (x, object (mine ++ others), others, mine)
                  | (x, (free, object)) <- objects,
                    -- Its method knows its own captures first, and then the others.
                    let mine = live env (free Set.\\ nameSet),
                    let others = filter (`Set.member` free) names
                ]
           in arrange env (same (kept ++ concat [mine | (_, _, _, mine) <- built])) (AxCut.Rec [(x, o, others) | (x, o, others, _) <- built] (continue (kept ++ names)))
      )
  other -> error ("Powder.AxCut.Lower: a statement that was not focused: " ++ take 200 (pretty (Program [] other ("", Halt) [])))
  where
    variable = \case
      Var x -> x
      _ -> error "Powder.AxCut.Lower: something used was not named"
    -- The same value under another name, which the old name may outlive.
    renamed new old (wanted, rest) =
      ( Set.insert old after,
        \env -> let kept = live env after in arrange env (same kept ++ [(new, old)]) (rest (kept ++ [new]))
      )
      where
        after = Set.delete new wanted

-- | The object a producer makes: the names it captures, and the object
-- given those names in the order they are to be captured in.
objectOf :: Producer -> M (Set.Set Name, [Name] -> AxCut.Object)
objectOf = \case
  Fun x a s -> closure AxCut.Closure AxCut.Ap [x, a] s
  Action ok err s -> closure AxCut.Closure AxCut.Run [ok, err] s
  Thunk a s -> closure AxCut.Thunk AxCut.Forced [a] s
  -- A constructor among things that refer to each other is a thunk of
  -- itself, since what it is made of may not be made yet.
  p -> do
    a <- fresh "k"
    x <- fresh "x"
    focus (Cut p (MuTilde x (Cut (Var x) (Covar a)))) >>= closure AxCut.Thunk AxCut.Forced [a]
  where
    closure kind method parameters s = do
      (free, body) <- linearly s
      pure (free Set.\\ Set.fromList parameters, \captured -> AxCut.Object kind captured (AxCut.Method method parameters (body (parameters ++ captured))))
