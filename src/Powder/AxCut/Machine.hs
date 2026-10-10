-- | A machine for AxCut: what the IR means, before anything is compiled
-- from it.
--
-- It is not the runtime. It is here to say what a program in AxCut does,
-- so that the lowering can be tested by itself against the combinator
-- reducer, and so that there is something small to read when a compiled
-- program and this disagree. It checks what a real machine would take on
-- trust: that the environment at each statement is exactly the row of
-- values the statement expects. A mismatch is a fault in the lowering,
-- and is reported as one.
--
-- It counts no references and frees nothing.
module Powder.AxCut.Machine (run) where

import Control.Exception
import Control.Monad
import Data.Char (chr, generalCategory, ord, toLower, toTitle, toUpper)
import Data.IORef
import Data.Ratio ((%))
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Data.Word (Word16, Word32, Word64, Word8)
import Powder.AxCut
import Powder.Core (ConInfo (..), Name, Prim (..), primName)
import Powder.Files (Files, newFiles)
import qualified Powder.Files as Files
import Powder.SKI.Eval (Hooks (..), RuntimeError (..))
import Powder.Foreign (callForeign)
import qualified Powder.Foreign as Foreign
import System.Exit

data Value
  = VInt Integer
  | VDouble Double
  | -- | A constructed value: which constructor of its type, and its fields.
    VData Int [Value]
  | VObject (IORef Cell)

-- | What an object is. A thunk is one of the last three in turn.
data Cell
  = -- | What was captured, and the method.
    Made [(Name, Value)] Method
  | Delayed [(Name, Value)] Method
  | -- | Being worked out: to need it now is to need it for itself.
    Working
  | Done Value
  | -- | The consumer a thunk's body is given: it keeps the value in the
    -- thunk, and passes it on to the consumer of the force.
    Update (IORef Cell) Value

data Machine = Machine
  { hooks :: Hooks,
    globals :: Map.Map Name (IORef Cell),
    files :: Files,
    cells :: IORef (Map.Map Integer Value),
    pushed :: IORef (Maybe (Maybe Char)),
    steps :: IORef Int
  }

failure :: String -> IO a
failure = throwIO . RuntimeError

-- | A fault in what made the program, not in the program.
broken :: String -> IO a
broken message = failure ("the AxCut program is malformed: " ++ message)

-- | Run a program, giving how many statements it took. Throws
-- 'RuntimeError', or 'ExitCode' if the program exits.
run :: Hooks -> Program -> IO Int
run h (Program defs entry) = do
  Foreign.loadLibraries >>= mapM_ failure . take 1
  made <- forM defs (\(x, k, s) -> (,) x <$> newIORef (Delayed [] (Method Forced [k] s)))
  m <- Machine h (Map.fromList made) <$> newFiles <*> newIORef Map.empty <*> newIORef Nothing <*> newIORef 0
  execute m [] entry
  readIORef (steps m)

-- | The environment: the values, the last first, each with the name the
-- program knows it by there.
type Env = [(Name, Value)]

-- | Take the last values off an environment, as many as some names, and
-- give them in order.
takeLast :: Int -> Env -> IO ([Value], Env)
takeLast n env
  | length env < n = broken "a statement takes more values than there are"
  | otherwise = pure (reverse (map snd (take n env)), drop n env)

push :: Env -> [(Name, Value)] -> Env
push env new = reverse new ++ env

execute :: Machine -> Env -> Statement -> IO ()
execute m env statement = do
  modifyIORef' (steps m) (+ 1)
  case statement of
    Halt -> unless (null env) (broken "the program ended with values left over")
    Substitute pairs s -> do
      new <- forM pairs $ \(to, from) -> case lookup from env of
        Just v -> pure (to, v)
        Nothing -> broken ("a substitution of " ++ from ++ ", which is not there")
      execute m (push [] new) s
    Let v c s -> do
      (fields, rest) <- takeLast (conArity c) env
      execute m (push rest [(v, VData (conTag c) fields)]) s
    Switch clauses -> case env of
      (_, VData tag fields) : rest -> case drop tag clauses of
        Clause c xs s : _
          | length xs == length fields -> execute m (push rest (zip xs fields)) s
          | otherwise -> broken ("a clause for " ++ conName c ++ " names the wrong number of fields")
        [] -> broken "a switch has no clause for a constructor"
      _ -> broken "a switch on what is not a constructed value"
    New v object s -> do
      (cell, rest) <- make object env
      ref <- newIORef cell
      execute m (push rest [(v, VObject ref)]) s
    Rec objects s -> do
      refs <- forM objects (const (newIORef Working))
      let named = [(v, VObject ref) | ((v, _, _), ref) <- zip objects refs]
      -- The first object's values are furthest from the end.
      rest <- foldM (fill named) env (reverse (zip objects refs))
      execute m (push rest named) s
    Lit v n s -> execute m (push env [(v, VInt n)]) s
    Global v g s -> case Map.lookup g (globals m) of
      Just ref -> execute m (push env [(v, VObject ref)]) s
      Nothing -> failure ("no definition of " ++ g)
    Prim v p n s -> do
      (arguments, rest) <- takeLast n env
      result <- primitive m p arguments
      execute m (push rest [(v, result)]) s
    Foreign v symbol shape result s -> do
      (arguments, rest) <- takeLast (length shape) env
      given <- forM (zip shape arguments) $ \case
        ('D', VDouble x) -> pure (Right x)
        ('F', VDouble x) -> pure (Right x)
        (_, VInt n) -> pure (Left (fromInteger n))
        _ -> failure "type error: a C function was given what is not a number"
      out <- callForeign symbol shape result given >>= either failure pure
      let value = case (result, out) of
            ('V', _) -> VData 0 []
            (_, Right x) -> VDouble x
            (_, Left n) -> VInt (narrowed result n)
      execute m (push rest [(v, value)]) s
    Force -> case env of
      [(_, x), (_, k)] -> force m x k
      _ -> broken "a force with more in the environment than a value and its consumer"
    Invoke method -> case env of
      (_, object) : arguments -> invoke m method object (reverse (map snd arguments))
      [] -> broken "an invoke with nothing in the environment"
  where
    make (Object kind captured method) from = do
      (values, rest) <- takeLast (length captured) from
      let held = zip captured values
      pure (if kind == Thunk then Delayed held method else Made held method, rest)
    -- An object among those that refer to each other: its own values off
    -- the environment, and then the others it names.
    fill named from ((_, Object kind captured method, others), ref) = do
      (mine, rest) <- takeLast (length captured - length others) from
      theirs <- forM others (\o -> maybe (broken ("no such object as " ++ o)) pure (lookup o named))
      let held = zip captured (mine ++ theirs)
      writeIORef ref (if kind == Thunk then Delayed held method else Made held method)
      pure rest

-- | Work a value out if it is a thunk, and give what it is to a consumer.
force :: Machine -> Value -> Value -> IO ()
force m x k = case x of
  VObject ref ->
    readIORef ref >>= \case
      Delayed captured (Method _ parameters body) -> do
        writeIORef ref Working
        update <- VObject <$> newIORef (Update ref k)
        enter m parameters [update] captured body
      Done v -> force m v k
      Working -> failure "a value depends on itself"
      _ -> invoke m Ret k [x]
  _ -> invoke m Ret k [x]

-- | Call a method of an object.
invoke :: Machine -> MethodName -> Value -> [Value] -> IO ()
invoke m method object arguments = case object of
  VObject ref ->
    readIORef ref >>= \case
      Made captured (Method name parameters body)
        | name == method -> enter m parameters arguments captured body
        | otherwise -> broken ("an object with the method " ++ show name ++ " was asked for " ++ show method)
      -- A thunk's value has come: keep it, and pass it on.
      Update thunk k | method == Ret, [v] <- arguments -> do
        writeIORef thunk (Done v)
        invoke m Ret k [v]
      -- A function, an action or a consumer may itself be not yet worked
      -- out only by a fault here: those are always forced first.
      _ -> broken ("a " ++ show method ++ " of what is not an object with that method")
  _ -> broken ("a " ++ show method ++ " of what is not an object")

-- | Run a method's body: its parameters, and then what it captured.
enter :: Machine -> [Name] -> [Value] -> [(Name, Value)] -> Statement -> IO ()
enter m parameters arguments captured body
  | length parameters /= length arguments = broken "a method was given the wrong number of arguments"
  | otherwise = execute m (push [] (zip parameters arguments ++ captured)) body

narrowed :: Char -> Int64 -> Integer
narrowed kind n = case kind of
  'I' -> toInteger (fromIntegral n :: Int32)
  'U' -> toInteger (fromIntegral n :: Word32)
  's' -> toInteger (fromIntegral n :: Int16)
  'S' -> toInteger (fromIntegral n :: Word16)
  'c' -> toInteger (fromIntegral n :: Int8)
  'C' -> toInteger (fromIntegral n :: Word8)
  'Q' -> toInteger (fromIntegral n :: Word64)
  _ -> toInteger n

-- | A primitive, of values already worked out.
primitive :: Machine -> Prim -> [Value] -> IO Value
primitive m p given = case (p, given) of
  (IntAdd, [VInt a, VInt b]) -> int (a + b)
  (IntSub, [VInt a, VInt b]) -> int (a - b)
  (IntMul, [VInt a, VInt b]) -> int (a * b)
  (IntQuot, [VInt a, VInt b]) -> if b == 0 then failure "division by zero" else int (quot a b)
  (IntRem, [VInt a, VInt b]) -> if b == 0 then failure "division by zero" else int (rem a b)
  (IntEq, [VInt a, VInt b]) -> bool (a == b)
  (IntLt, [VInt a, VInt b]) -> bool (a < b)
  (IntLe, [VInt a, VInt b]) -> bool (a <= b)
  (IntNarrow, [VInt a]) -> int (toInteger (fromInteger a :: Int64))
  (IntToDouble, [VInt a]) -> double (fromInteger a)
  (DoubleAdd, [VDouble a, VDouble b]) -> double (a + b)
  (DoubleSub, [VDouble a, VDouble b]) -> double (a - b)
  (DoubleMul, [VDouble a, VDouble b]) -> double (a * b)
  (DoubleDiv, [VDouble a, VDouble b]) -> double (a / b)
  (DoublePow, [VDouble a, VDouble b]) -> double (a ** b)
  (DoubleAtan2, [VDouble a, VDouble b]) -> double (atan2 a b)
  (DoubleEq, [VDouble a, VDouble b]) -> bool (a == b)
  (DoubleLt, [VDouble a, VDouble b]) -> bool (a < b)
  (DoubleLe, [VDouble a, VDouble b]) -> bool (a <= b)
  (DoubleTruncate, [VDouble a])
    | isNaN a || isInfinite a -> failure "a number that is not finite has no integer part"
    | otherwise -> int (truncate a)
  (DoubleFun, [VInt which, VDouble a]) ->
    case drop (fromInteger which) [exp, log, sqrt, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh] of
      f : _ | which >= 0 -> double (f a)
      _ -> failure "no such function of a double"
  (DoubleShowAt, [VDouble a, VInt i]) -> int (maybe (-1) (toInteger . ord) (lookup i (zip [0 ..] (show a))))
  (CharCategory, [VInt c]) -> int (toInteger (fromEnum (generalCategory (chr (fromInteger c)))))
  (CharUpper, [VInt c]) -> int (toInteger (ord (toUpper (chr (fromInteger c)))))
  (CharLower, [VInt c]) -> int (toInteger (ord (toLower (chr (fromInteger c)))))
  (CharTitle, [VInt c]) -> int (toInteger (ord (toTitle (chr (fromInteger c)))))
  (FloatShowAt, [VDouble a, VInt i]) -> int (maybe (-1) (toInteger . ord) (lookup i (zip [0 ..] (show (realToFrac a :: Float)))))
  (FloatNarrow, [VDouble a]) -> double (realToFrac (realToFrac a :: Float))
  (DoubleMantissa, [VDouble a]) -> int (fst (decodeFloat a))
  (DoubleExponent, [VDouble a]) -> int (toInteger (snd (decodeFloat a)))
  (DoubleFromRatio, [VInt a, VInt b]) -> double (if b == 0 then (if a == 0 then 0 / 0 else fromInteger (signum a) / 0) else fromRational (a % b))
  (DoubleEncode, [VInt a, VInt e]) -> double (encodeFloat a (fromInteger (max (-100000) (min 100000 e))))
  (MatchFail, []) -> failure "pattern match failure"
  (PutChar, [VInt h, VInt c]) -> do
    written <- Files.put (files m) h (chr (fromInteger c))
    unit <$ unless written (putCh (hooks m) (fromInteger h) (chr (fromInteger c)))
  (GetChar, []) -> standardInput >>= code
  (Exit, [VInt n]) -> throwIO (if n == 0 then ExitSuccess else ExitFailure (fromInteger n))
  (PathChar, [VInt c]) -> unit <$ Files.spell (files m) (chr (fromInteger c))
  (FileOpen, [VInt mode]) -> Files.open (files m) mode >>= int
  (HGetChar, [VInt h]) -> Files.get (files m) h >>= code
  (FileClose, [VInt h]) -> unit <$ Files.close (files m) h
  (HIsEOF, [VInt h]) ->
    Files.atEnd (files m) h >>= \case
      Just done -> bool done
      Nothing -> do
        c <- standardInput
        writeIORef (pushed m) (Just c)
        bool (isNothing c)
  (FileSeek, [VInt h, VInt whence, VInt offset]) -> Files.seek (files m) h whence offset >>= int
  (FileTruncate, [VInt h, VInt size]) -> Files.resize (files m) h size >>= int
  (FileFlush, [VInt h]) -> unit <$ Files.flush (files m) h
  (IsTerminal, [VInt h]) -> Files.terminal (files m) h >>= bool
  (EnvChar, [VInt i, VInt j]) -> Files.environment i j >>= code
  (CellNew, [x]) -> do
    n <- toInteger . Map.size <$> readIORef (cells m)
    modifyIORef' (cells m) (Map.insert n x)
    int n
  (CellRead, [VInt n]) -> readIORef (cells m) >>= maybe (failure "there is no such cell") pure . Map.lookup n
  (CellWrite, [VInt n, x]) -> unit <$ modifyIORef' (cells m) (Map.insert n x)
  (PeekInt, [VInt kind, VInt address]) -> Foreign.peekWhole kind address >>= int
  (PeekDouble, [VInt kind, VInt address]) -> Foreign.peekReal kind address >>= double
  (PokeInt, [VInt kind, VInt address, VInt value]) -> unit <$ Foreign.pokeWhole kind address value
  (PokeDouble, [VInt kind, VInt address, VDouble value]) -> unit <$ Foreign.pokeReal kind address value
  (ErrnoGet, []) -> Foreign.getErrno >>= int
  (ErrnoSet, [VInt value]) -> unit <$ Foreign.setErrno value
  (ArgCount, []) -> int (toInteger (length (arguments (hooks m))))
  (ArgChar, [VInt i, VInt j]) -> do
    let text = if i < 0 then programName (hooks m) else concat (take 1 (drop (fromInteger i) (arguments (hooks m))))
    int (maybe (-1) (toInteger . ord) (lookup j (zip [0 ..] text)))
  _ -> failure ("type error: " ++ primName p ++ " was given what it cannot take")
  where
    int = pure . VInt
    code = int . maybe (-1) (toInteger . ord)
    double = pure . VDouble
    -- The constructors of Bool, and of the unit.
    bool b = pure (VData (if b then 1 else 0) [])
    unit = VData 0 []
    standardInput =
      readIORef (pushed m) >>= \case
        Just c -> c <$ writeIORef (pushed m) Nothing
        Nothing -> getCh (hooks m)
