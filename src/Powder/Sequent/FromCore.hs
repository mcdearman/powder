-- | Core to the sequent calculus.
--
-- An expression is translated two ways, by what is wanted of it. To
-- /evaluate/ it is a statement that works it out as far as its outermost
-- constructor or lambda and gives that to a consumer. To have it as a
-- /value/ is a producer of something that can be passed about without
-- being worked out: a thunk of its evaluation, unless it is already one
-- of the things that need no working out, which are a variable, a number,
-- a lambda, an action and a constructor applied to all its fields. Those
-- are never thunks, which is the first thing done here about laziness.
--
-- Actions are compiled away. In Core an action is a primitive applied and
-- left for the runtime to interpret; here it is something to run, given a
-- consumer for its result and one for what it raises, and sequencing,
-- raising and catching are just how those consumers are passed along.
module Powder.Sequent.FromCore (fromCore) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import Powder.Core (ConInfo (..), Core, Prim (..))
import qualified Powder.Core as Core
import Powder.Sequent

-- | The names in scope that are not definitions at the top, each with the
-- name it has here, where no two things have the same name; and a supply
-- of new names.
type M = ReaderT (Map.Map Name Name) (State Int)

-- | Translate a program, to be started by running the action a
-- definition is.
fromCore :: Name -> Core.Program -> Program
fromCore root (Core.Program _ defs) = evalState (runReaderT build Map.empty) 0
  where
    build = do
      definitions <- forM defs $ \(x, e) -> (,) x <$> delayed e
      Program definitions <$> entry
    -- Run the action; what it raises and nothing catches says what to do.
    entry = do
      r <- fresh "r"
      u <- fresh "u"
      v <- fresh "v"
      r' <- fresh "r"
      e <- fresh "e"
      let finish = MuTilde r Halt
          uncaught = Case [Alt raised [u, v] (Cut (Var u) (Force (Run (MuTilde r' Halt) (MuTilde e Halt))))]
      pure (Cut (Global root) (Force (Run finish uncaught)))

fresh :: String -> M Name
fresh hint = state (\n -> (hint ++ "%" ++ show n, n + 1))

-- | Bring a name of Core's into scope, under a name of its own.
binding :: Name -> (Name -> M a) -> M a
binding x inner = do
  x' <- fresh (takeWhile (/= '%') x)
  local (Map.insert x x') (inner x')

bindings :: [Name] -> ([Name] -> M a) -> M a
bindings [] inner = inner []
bindings (x : xs) inner = binding x (\x' -> bindings xs (inner . (x' :)))

-- | A thunk of an expression's evaluation, whatever the expression is: a
-- definition at the top is always one.
delayed :: Core -> M Producer
delayed e = do
  a <- fresh "k"
  Thunk a <$> evaluate e (Covar a)

-- | An expression as something that can be passed about.
value :: Core -> M Producer
value e = case spine e [] of
  (Core.Var x, []) -> asks (maybe (Global x) Var . Map.lookup x)
  (Core.Lit n, []) -> pure (Lit n)
  (Core.Lam x body, []) -> function x body
  (Core.Con c, arguments) | length arguments == conArity c -> Con c <$> mapM value arguments
  (Core.Prim p, arguments) | Just make <- action p, length arguments == arity p -> make arguments
  _ -> delayed e

function :: Name -> Core -> M Producer
function x body = binding x $ \x' -> do
  a <- fresh "k"
  Fun x' a <$> evaluate body (Covar a)

-- | A function applied, as what is at its head and what that is given.
spine :: Core -> [Core] -> (Core, [Core])
spine e arguments = case e of
  Core.App f a -> spine f (a : arguments)
  _ -> (e, arguments)

-- | Work an expression out and give it to a consumer.
evaluate :: Core -> Consumer -> M Statement
evaluate e k = case spine e [] of
  (Core.Var _, []) -> (`Cut` Force k) <$> value e
  (Core.Lit n, []) -> pure (Cut (Lit n) k)
  (Core.Lam x body, []) -> (`Cut` k) <$> function x body
  (Core.Let (Core.NonRec x bound) body, []) -> do
    bound' <- value bound
    binding x $ \x' -> Cut bound' . MuTilde x' <$> evaluate body k
  (Core.Let (Core.Rec bs) body, []) -> bindings (map fst bs) $ \xs -> do
    -- Each must be something made, for the others to refer to.
    made <- forM bs $ \(_, bound) -> do
      p <- value bound
      case p of
        Var _ -> delayed bound
        Global _ -> delayed bound
        Lit _ -> delayed bound
        _ -> pure p
    LetRec (zip xs made) <$> evaluate body k
  (Core.Case scrutinee alts, []) -> shared k $ \k' -> do
    alts' <- forM alts $ \(Core.Alt c xs body) -> bindings xs (\xs' -> Alt c xs' <$> evaluate body k')
    evaluate scrutinee (Case alts')
  -- A constructor with all its fields is a value. With too few it is a
  -- function of the rest.
  (Core.Con c, arguments)
    | length arguments >= conArity c -> do
        let (fields, rest) = splitAt (conArity c) arguments
        built <- Con c <$> mapM value fields
        Cut built <$> applied rest k
    | otherwise -> expanded e (conArity c - length arguments) k
  (Core.Prim p, arguments)
    | length arguments >= arity p -> do
        let (mine, rest) = splitAt (arity p) arguments
        applied rest k >>= primitive p mine
    | otherwise -> expanded e (arity p - length arguments) k
  (Core.Foreign symbol shape result, arguments)
    | length arguments >= length shape -> do
        let (mine, rest) = splitAt (length shape) arguments
        k' <- applied rest k
        strictly mine (\vs -> pure (Foreign symbol shape result vs k'))
    | otherwise -> expanded e (length shape - length arguments) k
  (f, arguments) -> applied arguments k >>= evaluate' f
  where
    evaluate' f k' = case f of
      Core.App _ _ -> error "Powder.Sequent.FromCore: a spine with an application at its head"
      _ -> evaluate f k'

-- | The consumer of a function that applies it to arguments, one after
-- another, and gives what comes of that to a consumer.
applied :: [Core] -> Consumer -> M Consumer
applied arguments k = foldM (\rest a -> (`Ap` rest) <$> value a) k (reverse arguments)

-- | Something given fewer arguments than it takes is a function of the
-- rest.
expanded :: Core -> Int -> Consumer -> M Statement
expanded e missing k = do
  xs <- replicateM missing (fresh "eta")
  let whole = foldl Core.App e (map Core.Var xs)
  evaluate (foldr Core.Lam whole xs) k

-- | Use a consumer more than once: if it is more than a name, give it one.
shared :: Consumer -> (Consumer -> M Statement) -> M Statement
shared k inner = case k of
  Covar _ -> inner k
  _ -> do
    a <- fresh "j"
    (`Cut` k) . Mu a <$> inner (Covar a)

-- | Work some expressions out, in order, and go on with them as values.
strictly :: [Core] -> ([Producer] -> M Statement) -> M Statement
strictly [] inner = inner []
strictly (e : es) inner = do
  x <- fresh "v"
  rest <- strictly es (inner . (Var x :))
  evaluate e (MuTilde x rest)

-- | How many arguments a primitive takes.
arity :: Prim -> Int
arity = \case
  MatchFail -> 0
  GetChar -> 0
  ArgCount -> 0
  FileSeek -> 3
  p | p `elem` [Seq, Bind, PutChar, Throw, Catch, ArgChar, FileTruncate, EnvChar, CellWrite, DoubleShowAt, FloatShowAt, DoubleFun, DoubleEncode, DoubleFromRatio] -> 2
  p | p `elem` [IntAdd, IntSub, IntMul, IntQuot, IntRem, IntEq, IntLt, IntLe] -> 2
  p | p `elem` [DoubleAdd, DoubleSub, DoubleMul, DoubleDiv, DoublePow, DoubleAtan2, DoubleEq, DoubleLt, DoubleLe] -> 2
  _ -> 1

-- | A primitive that is an action, as one: given its arguments, something
-- to run.
action :: Prim -> Maybe ([Core] -> M Producer)
action p = case p of
  Return -> Just $ \arguments -> act $ \ok _ -> case arguments of
    [x] -> (`Cut` Covar ok) <$> value x
    _ -> wrong
  -- Run the first, and then run what the second makes of its result.
  Bind -> Just $ \arguments -> act $ \ok err -> case arguments of
    [m, f] -> do
      x <- fresh "x"
      next <- local (Map.insert x x) (evaluate (Core.App f (Core.Var x)) (Run (Covar ok) (Covar err)))
      evaluate m (Run (MuTilde x next) (Covar err))
    _ -> wrong
  -- What is raised goes to whatever catches, with what to do if nothing does.
  Throw -> Just $ \arguments -> act $ \_ err -> case arguments of
    [uncaught, x] -> (\u v -> Cut (Con raised [u, v]) (Covar err)) <$> value uncaught <*> value x
    _ -> wrong
  -- Run the first with the second as what catches.
  Catch -> Just $ \arguments -> act $ \ok err -> case arguments of
    [m, handler] -> do
      u <- fresh "u"
      x <- fresh "x"
      handle <- local (Map.insert x x) (evaluate (Core.App handler (Core.Var x)) (Run (Covar ok) (Covar err)))
      evaluate m (Run (Covar ok) (Case [Alt raised [u, x] handle]))
    _ -> wrong
  _ | p `elem` effects -> Just $ \arguments -> act $ \ok _ -> strictly arguments (\vs -> pure (Prim p vs (Covar ok)))
  _ -> Nothing
  where
    act body = do
      ok <- fresh "ok"
      err <- fresh "err"
      Action ok err <$> body ok err
    wrong = error "Powder.Sequent.FromCore: an action given the wrong number of arguments"
    effects = [PutChar, GetChar, Exit, PathChar, FileOpen, HGetChar, FileClose, HIsEOF, ArgCount, ArgChar, FileSeek, FileTruncate, FileFlush, IsTerminal, EnvChar, CellNew, CellRead, CellWrite]

-- | A primitive given all its arguments, with where its result goes.
primitive :: Prim -> [Core] -> Consumer -> M Statement
primitive p arguments k = case (p, arguments) of
  _ | Just make <- action p -> (`Cut` k) <$> make arguments
  -- These are the same thing at another type.
  (Coerce, [x]) -> evaluate x k
  (CharToInt, [x]) -> evaluate x k
  (IntToChar, [x]) -> evaluate x k
  (Seq, [a, b]) -> do
    x <- fresh "v"
    evaluate a . MuTilde x =<< evaluate b k
  -- Run an action for its result, which is then worked out. What it
  -- raises is not caught: it ends the program.
  (PerformIO, [m]) -> do
    x <- fresh "x"
    u <- fresh "u"
    v <- fresh "v"
    r <- fresh "r"
    e <- fresh "e"
    let uncaught = Case [Alt raised [u, v] (Cut (Var u) (Force (Run (MuTilde r Halt) (MuTilde e Halt))))]
    evaluate m (Run (MuTilde x (Cut (Var x) (Force k))) uncaught)
  _ -> strictly arguments (\vs -> pure (Prim p vs k))
