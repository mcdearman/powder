-- | A graph reducer for combinator programs.
module Powder.SKI.Eval
  ( Hooks (..),
    stdHooks,
    RuntimeError (..),
    run,
    Value (..),
    eval,
  )
where

import Control.Exception
import Control.Monad
import Data.Char (chr, generalCategory, ord, toLower, toTitle, toUpper)
import Data.Functor ((<&>))
import Data.IORef
import Data.Ratio ((%))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import Powder.Foreign (callForeign)
import qualified Powder.Foreign as Foreign
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Powder.Core (Name, Prim (..))
import Powder.Files (Files, newFiles)
import qualified Powder.Files as Files
import Powder.SKI.Comb
import System.Exit
import System.IO

-- | The program is a mutable graph. Reducing an application overwrites its
-- node with the result, so everything sharing the node shares the work:
-- that is what makes evaluation lazy rather than merely call-by-name.
type Ref = IORef Node

data Node
  = NApp !Ref !Ref
  | -- | An indirection, left where a reduction's result is an existing node.
    NInd !Ref
  | NInt !Integer
  | NDbl !Double
  | NComb !Combinator
  | NPrim !Prim
  | -- | A C function: its name, its arguments' kinds and its result's.
    NForeign String String Char

-- | How the program talks to the outside world.
data Hooks = Hooks
  { -- | Write a character to a handle: 1 is stdout, 2 is stderr.
    putCh :: Int -> Char -> IO (),
    -- | Read a character, or 'Nothing' at end of input.
    getCh :: IO (Maybe Char),
    -- | What the program is called, and the arguments it was given.
    programName :: String,
    arguments :: [String]
  }

-- | The process's own stdin, stdout and stderr.
stdHooks :: Hooks
stdHooks =
  Hooks
    { putCh = \h -> hPutChar (if h == 2 then stderr else stdout),
      getCh = do
        hFlush stdout
        eof <- isEOF
        if eof then pure Nothing else Just <$> getChar,
      programName = "powder",
      arguments = []
    }

newtype RuntimeError = RuntimeError String
  deriving (Show)

instance Exception RuntimeError

-- | Something a program raised: what to do if nothing catches it, and
-- what was raised.
data Raised = Raised Ref Ref

instance Show Raised where
  show _ = "an error a program raised"

instance Exception Raised

data Machine = Machine
  { hooks :: Hooks,
    steps :: IORef Int,
    files :: Files,
    -- | The program's cells, each a place for a value that can be changed.
    cells :: IORef (Map.Map Integer Ref),
    -- | A character read from the standard input to see whether there was
    -- one, and not yet taken.
    pushed :: IORef (Maybe (Maybe Char))
  }

failure :: String -> IO a
failure = throwIO . RuntimeError

-- | Run @main@ and return the number of reductions performed. Throws
-- 'RuntimeError', or 'ExitCode' if the program exits.
run :: Hooks -> [(Name, Comb)] -> IO Int
run h defs = do
  (m, entry) <- start h defs "main"
  _ <- runIO m entry `catch` \(Raised uncaught _) -> runIO m uncaught
  readIORef (steps m)

-- | What can be said about a value by looking at it. Data is Scott-encoded,
-- so it looks like any other function: without types, only integers and IO
-- actions are recognisable.
data Value
  = VInt Integer
  | -- | An IO action, which has been run, and its result if an integer.
    VAction (Maybe Integer)
  | VOther

-- | Evaluate a definition the way a REPL wants: to weak head normal form,
-- and then run it if it turns out to be an IO action.
eval :: Hooks -> [(Name, Comb)] -> Name -> IO Value
eval h defs root = do
  (m, entry) <- start h defs root
  r <- whnf m entry
  action <- spine r [] <&> \case
    (NPrim p, args) -> lookup p ioArity == Just (length args)
    _ -> False
  if action
    then VAction <$> ((runIO m r `catch` \(Raised uncaught _) -> runIO m uncaught) >>= whnf m >>= asInt)
    else maybe VOther VInt <$> asInt r
  where
    asInt r = readIORef r <&> \case NInt n -> Just n; _ -> Nothing
    ioArity = [(Return, 1), (Bind, 2), (PutChar, 2), (GetChar, 0), (Exit, 1), (PathChar, 1), (Throw, 2), (Catch, 2), (FileOpen, 1), (HGetChar, 1), (FileClose, 1), (HIsEOF, 1), (ArgCount, 0), (ArgChar, 2), (FileSeek, 3), (FileTruncate, 2), (FileFlush, 1), (IsTerminal, 1), (EnvChar, 2), (CellNew, 1), (CellRead, 1), (CellWrite, 2), (PeekInt, 2), (PeekDouble, 2), (PokeInt, 3), (PokeDouble, 3), (ErrnoGet, 0), (ErrnoSet, 1)]

start :: Hooks -> [(Name, Comb)] -> Name -> IO (Machine, Ref)
start h defs root = do
  Foreign.loadLibraries >>= mapM_ failure . take 1
  globals <- link defs
  entry <- maybe (failure ("no definition of " ++ root)) pure (Map.lookup root globals)
  m <- Machine h <$> newIORef 0 <*> newFiles <*> newIORef Map.empty <*> newIORef Nothing
  pure (m, entry)

-- | Build the graph. Each definition gets one node, which every reference
-- to it points at, so definitions are shared and may be cyclic.
link :: [(Name, Comb)] -> IO (Map.Map Name Ref)
link defs = do
  globals <- Map.fromList <$> mapM (\(x, _) -> (,) x <$> newIORef (NInt 0)) defs
  let build = \case
        CVar x -> maybe (failure ("unbound variable " ++ x)) pure (Map.lookup x globals)
        CInt n -> newIORef (NInt n)
        CPrim p -> newIORef (NPrim p)
        CForeign symbol arguments result -> newIORef (NForeign symbol arguments result)
        CComb c -> newIORef (NComb c)
        f :@ a -> NApp <$> build f <*> build a >>= newIORef
  forM_ defs $ \(x, e) -> build e >>= writeIORef (globals Map.! x) . NInd
  pure globals

-- | Reduce a node to weak head normal form and return it, with indirections
-- followed.
whnf :: Machine -> Ref -> IO Ref
whnf m root = do
  r <- unwind root []
  when (r /= root) $ writeIORef root (NInd r)
  pure r
  where
    -- Walk down the spine, stacking each application node with its argument,
    -- until the head says what to do. The n-th stack entry holds the n-th
    -- argument, and is the root of the redex for a head of arity n.
    unwind r stk =
      readIORef r >>= \case
        NInd r' -> unwind r' stk
        NApp f a -> unwind f ((r, a) : stk)
        NInt _
          | null stk -> pure r
          | otherwise -> failure "type error: an integer was applied to an argument"
        NDbl _
          | null stk -> pure r
          | otherwise -> failure "type error: a floating-point number was applied to an argument"
        NComb c -> comb c r stk
        NPrim p -> prim p r stk
        -- A C function is called when all its arguments are there, each
        -- worked out first.
        NForeign symbol shape result
          | length stk < length shape -> stuck r stk
          | otherwise -> do
              let (mine, rest) = splitAt (length shape) stk
              given' <- forM (zip shape mine) $ \(k, (_, a)) ->
                if k `elem` "DF" then Right <$> double m a else Left . fromInteger <$> int m a
              out <- callForeign symbol shape result given' >>= either failure pure
              let node = case (result, out) of
                    ('V', _) -> NComb I
                    (_, Right x) -> NDbl x
                    (_, Left n) -> NInt (narrowed result n)
              update (case reverse mine of (redex, _) : _ -> redex; [] -> r) node rest

    -- Too few arguments: the expression is already in weak head normal form.
    stuck r stk = pure (if null stk then r else fst (last stk))

    update n node stk = tick >> writeIORef n node >> unwind n stk
    redirect n r stk = tick >> writeIORef n (NInd r) >> unwind r stk
    tick = modifyIORef' (steps m) (+ 1)
    app f a = newIORef (NApp f a)

    comb c r stk = case (c, stk) of
      (I, (n, x) : s) -> redirect n x s
      (K, (_, x) : (n, _) : s) -> redirect n x s
      (S, (_, f) : (_, g) : (n, x) : s) -> do
        fx <- app f x
        gx <- app g x
        update n (NApp fx gx) s
      (B, (_, f) : (_, g) : (n, x) : s) -> do
        gx <- app g x
        update n (NApp f gx) s
      (C, (_, f) : (_, g) : (n, x) : s) -> do
        fx <- app f x
        update n (NApp fx g) s
      (S', (_, p) : (_, f) : (_, g) : (n, x) : s) -> do
        fx <- app f x
        gx <- app g x
        pfx <- app p fx
        update n (NApp pfx gx) s
      (B', (_, p) : (_, f) : (_, g) : (n, x) : s) -> do
        pf <- app p f
        gx <- app g x
        update n (NApp pf gx) s
      (C', (_, p) : (_, f) : (_, g) : (n, x) : s) -> do
        fx <- app f x
        pfx <- app p fx
        update n (NApp pfx g) s
      (C'B, (_, p) : (_, f) : (_, g) : (n, x) : s) -> do
        pg <- app p g
        fx <- app f x
        update n (NApp pg fx) s
      (A, _ : (n, y) : s) -> redirect n y s
      (U, (_, x) : (n, y) : s) -> update n (NApp y x) s
      (Z, (_, x) : (_, y) : (n, _) : s) -> update n (NApp x y) s
      (P, (_, x) : (_, y) : (n, z) : s) -> do
        zx <- app z x
        update n (NApp zx y) s
      (R, (_, x) : (_, y) : (n, z) : s) -> do
        yz <- app y z
        update n (NApp yz x) s
      (O, (_, x) : (_, y) : _ : (n, w) : s) -> do
        wx <- app w x
        update n (NApp wx y) s
      (K2, (_, x) : _ : (n, _) : s) -> redirect n x s
      (K3, (_, x) : _ : _ : (n, _) : s) -> redirect n x s
      (K4, (_, x) : _ : _ : _ : (n, _) : s) -> redirect n x s
      -- Tie the knot: the result of Y f is its own argument.
      (Y, (n, f) : s) -> update n (NApp f n) s
      _ -> stuck r stk

    prim p r stk = case p of
      IntAdd -> arith (+)
      IntSub -> arith (-)
      IntMul -> arith (*)
      IntQuot -> division quot
      IntRem -> division rem
      IntEq -> comparison (==)
      IntLt -> comparison (<)
      IntLe -> comparison (<=)
      Seq | (_, a) : (n, b) : s <- stk -> whnf m a >> redirect n b s
      PerformIO | (n, a) : s <- stk -> runIO m a >>= \x -> redirect n x s
      MatchFail -> failure "pattern match failure"
      CharToInt | (n, a) : s <- stk -> redirect n a s
      IntToChar | (n, a) : s <- stk -> redirect n a s
      Coerce | (n, a) : s <- stk -> redirect n a s
      IntNarrow | (n, a) : s <- stk -> int m a >>= \x -> update n (NInt (toInteger (fromInteger x :: Int64))) s
      FloatNarrow | (n, a) : s <- stk -> double m a >>= \x -> update n (NDbl (realToFrac (realToFrac x :: Float))) s
      CharCategory | (n, a) : s <- stk -> int m a >>= \c -> update n (NInt (toInteger (fromEnum (generalCategory (chr (fromInteger c)))))) s
      CharUpper | (n, a) : s <- stk -> int m a >>= \c -> update n (NInt (toInteger (ord (toUpper (chr (fromInteger c)))))) s
      CharLower | (n, a) : s <- stk -> int m a >>= \c -> update n (NInt (toInteger (ord (toLower (chr (fromInteger c)))))) s
      CharTitle | (n, a) : s <- stk -> int m a >>= \c -> update n (NInt (toInteger (ord (toTitle (chr (fromInteger c)))))) s
      FloatShowAt | (_, a) : (n, i) : s <- stk -> do
        x <- double m a
        at <- int m i
        update n (NInt (maybe (-1) (toInteger . ord) (lookup at (zip [0 ..] (show (realToFrac x :: Float)))))) s
      IntToDouble | (n, a) : s <- stk -> int m a >>= \x -> update n (NDbl (fromInteger x)) s
      DoubleAdd -> floating (+)
      DoubleSub -> floating (-)
      DoubleMul -> floating (*)
      DoubleDiv -> floating (/)
      DoublePow -> floating (**)
      DoubleAtan2 -> floating atan2
      DoubleEq -> comparing (==)
      DoubleLt -> comparing (<)
      DoubleLe -> comparing (<=)
      DoubleTruncate | (n, a) : s <- stk -> do
        x <- double m a
        if isNaN x || isInfinite x then failure "a number that is not finite has no integer part" else update n (NInt (truncate x)) s
      DoubleFun | (_, k) : (n, a) : s <- stk -> do
        which <- int m k
        x <- double m a
        let functions = [exp, log, sqrt, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh]
        case drop (fromInteger which) functions of
          f : _ | which >= 0 -> update n (NDbl (f x)) s
          _ -> failure "no such function of a double"
      DoubleShowAt | (_, a) : (n, i) : s <- stk -> do
        x <- double m a
        at <- int m i
        update n (NInt (maybe (-1) (toInteger . ord) (lookup at (zip [0 ..] (show x))))) s
      DoubleMantissa | (n, a) : s <- stk -> double m a >>= \x -> update n (NInt (fst (decodeFloat x))) s
      DoubleExponent | (n, a) : s <- stk -> double m a >>= \x -> update n (NInt (toInteger (snd (decodeFloat x)))) s
      DoubleFromRatio | (_, a) : (n, b) : s <- stk -> do
        x <- int m a
        y <- int m b
        update n (NDbl (if y == 0 then (if x == 0 then 0 / 0 else fromInteger (signum x) / 0) else fromRational (x % y))) s
      DoubleEncode | (_, a) : (n, b) : s <- stk -> do
        mantissa <- int m a
        e <- int m b
        update n (NDbl (encodeFloat mantissa (fromInteger (max (-100000) (min 100000 e))))) s
      -- IO primitives are inert until runIO executes them.
      _ -> stuck r stk
      where
        binary f = case stk of
          (_, a) : (n, b) : s -> do
            x <- int m a
            y <- int m b
            node <- f x y
            update n node s
          _ -> stuck r stk
        arith f = binary (\x y -> pure (NInt (f x y)))
        binaryD f = case stk of
          (_, a) : (n, b) : s -> do
            x <- double m a
            y <- double m b
            update n (f x y) s
          _ -> stuck r stk
        floating f = binaryD (\x y -> NDbl (f x y))
        comparing f = binaryD (\x y -> NComb (if f x y then A else K))
        division f = binary $ \x y ->
          if y == 0 then failure "division by zero" else pure (NInt (f x y))
        -- The Scott encodings of False and True.
        comparison f = binary (\x y -> pure (NComb (if f x y then A else K)))

double :: Machine -> Ref -> IO Double
double m r =
  whnf m r >>= readIORef >>= \case
    NDbl x -> pure x
    _ -> failure "type error: expected a floating-point number"

-- | An integer C gave back, as the type it was said to be: C leaves what
-- is past a narrow result's width as it finds it.
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

int :: Machine -> Ref -> IO Integer
int m r =
  whnf m r >>= readIORef >>= \case
    NInt n -> pure n
    _ -> failure "type error: expected an integer"

-- | Execute an IO action and return its result. An action is an IO
-- primitive applied to its arguments; reduction leaves those alone, and
-- this interprets them.
runIO :: Machine -> Ref -> IO Ref
runIO m r = do
  action <- whnf m r >>= \r' -> spine r' []
  case action of
    (NPrim Return, [x]) -> pure x
    (NPrim Bind, [a, k]) -> do
      x <- runIO m a
      newIORef (NApp k x) >>= runIO m
    (NPrim PutChar, [h, c]) -> do
      h' <- int m h
      c' <- chr . fromInteger <$> int m c
      written <- Files.put (files m) h' c'
      unless written (putCh (hooks m) (fromInteger h') c')
      unit
    (NPrim Throw, [uncaught, raised]) -> throwIO (Raised uncaught raised)
    (NPrim Catch, [action', handler]) ->
      runIO m action' `catch` \(Raised _ raised) -> newIORef (NApp handler raised) >>= runIO m
    (NPrim PathChar, [c]) -> do
      c' <- int m c
      Files.spell (files m) (chr (fromInteger c'))
      unit
    (NPrim FileOpen, [mode]) -> int m mode >>= Files.open (files m) >>= number
    (NPrim HGetChar, [h]) -> int m h >>= Files.get (files m) >>= code
    (NPrim FileClose, [h]) -> int m h >>= Files.close (files m) >> unit
    (NPrim GetChar, []) -> standardInput >>= code
    (NPrim HIsEOF, [h]) -> do
      h' <- int m h
      Files.atEnd (files m) h' >>= \case
        Just done -> truth done
        -- To see whether there is a character is to read it, and keep it.
        Nothing -> do
          c <- standardInput
          writeIORef (pushed m) (Just c)
          truth (isNothing c)
    (NPrim FileSeek, [h, whence, offset]) -> do
      h' <- int m h
      whence' <- int m whence
      int m offset >>= Files.seek (files m) h' whence' >>= number
    (NPrim FileTruncate, [h, to]) -> do
      h' <- int m h
      int m to >>= Files.resize (files m) h' >>= number
    (NPrim FileFlush, [h]) -> int m h >>= Files.flush (files m) >> unit
    (NPrim IsTerminal, [h]) -> int m h >>= Files.terminal (files m) >>= truth
    (NPrim EnvChar, [i, j]) -> do
      i' <- int m i
      int m j >>= Files.environment i' >>= code
    (NPrim CellNew, [x]) -> do
      n <- toInteger . Map.size <$> readIORef (cells m)
      modifyIORef' (cells m) (Map.insert n x)
      number n
    (NPrim CellRead, [n]) -> do
      n' <- int m n
      readIORef (cells m) >>= maybe (failure "there is no such cell") pure . Map.lookup n'
    (NPrim CellWrite, [n, x]) -> do
      n' <- int m n
      modifyIORef' (cells m) (Map.insert n' x)
      unit
    (NPrim PeekInt, [kind, address]) -> do
      kind' <- int m kind
      int m address >>= Foreign.peekWhole kind' >>= number
    (NPrim PeekDouble, [kind, address]) -> do
      kind' <- int m kind
      int m address >>= Foreign.peekReal kind' >>= newIORef . NDbl
    (NPrim PokeInt, [kind, address, value]) -> do
      kind' <- int m kind
      address' <- int m address
      int m value >>= Foreign.pokeWhole kind' address' >> unit
    (NPrim PokeDouble, [kind, address, value]) -> do
      kind' <- int m kind
      address' <- int m address
      double m value >>= Foreign.pokeReal kind' address' >> unit
    (NPrim ErrnoGet, []) -> Foreign.getErrno >>= number
    (NPrim ErrnoSet, [value]) -> int m value >>= Foreign.setErrno >> unit
    (NPrim ArgCount, []) -> newIORef (NInt (toInteger (length (arguments (hooks m)))))
    (NPrim ArgChar, [i, j]) -> do
      i' <- int m i
      j' <- int m j
      let text = if i' < 0 then programName (hooks m) else concat (take 1 (drop (fromInteger i') (arguments (hooks m))))
      newIORef (NInt (maybe (-1) (toInteger . ord) (lookup j' (zip [0 ..] text))))
    (NPrim Exit, [n]) -> do
      n' <- int m n
      throwIO (if n' == 0 then ExitSuccess else ExitFailure (fromInteger n'))
    _ -> failure "type error: expected an IO action"
  where
    -- The Scott encoding of ().
    unit = newIORef (NComb I)
    number = newIORef . NInt
    code = number . maybe (-1) (toInteger . ord)
    truth done = newIORef (NComb (if done then A else K))
    standardInput =
      readIORef (pushed m) >>= \case
        Just c -> c <$ writeIORef (pushed m) Nothing
        Nothing -> getCh (hooks m)

-- | The head of an application and its arguments.
spine :: Ref -> [Ref] -> IO (Node, [Ref])
spine r args =
  readIORef r >>= \case
    NApp f a -> spine f (a : args)
    NInd r' -> spine r' args
    node -> pure (node, args)
