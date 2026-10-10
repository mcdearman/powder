-- | Bytecode: AxCut compiled for a machine with registers.
--
-- AxCut names every value by where it is in a row that each statement
-- makes again. Here a value is in a register, and stays in it: the row is
-- only how this module knows which register a name is in. So making the
-- row again costs nothing where it only renames; a value used twice is
-- copied to a register of its own, and one no longer wanted is dropped,
-- and those are the only instructions a @substitute@ becomes. Everything
-- else takes what it uses out of its registers, since in AxCut a value is
-- used once.
--
-- Code is in blocks, each a run of instructions that ends by going
-- somewhere else: the body of a method, or a clause of a switch. A
-- method's block starts with its parameters in the first registers and
-- what its object captured in the ones after; a clause's starts with
-- every register as the switch left it, and the fields where the switch
-- was told to put them. Before a method is called its arguments are
-- moved into the first registers, and nothing else is alive: a call does
-- not come back. What comes back is a consumer being called, and a
-- consumer is not an object on the heap but a frame on the machine's
-- stack of them: an object whose method is the one that takes a value.
--
-- An image is, in order:
--
-- > "PWDR", version
-- > strings      the names of primitives and C functions, and numbers too
-- >              big for sixty-four bits, in decimal
-- > registers    how many the program uses
-- > blocks       each its instructions
-- > definitions  each the block that works it out
-- > entry        the block that starts the program
-- > callback     the block that answers a call from C
-- > exports      the functions C may call
--
-- Every number is written seven bits to a byte, least first, with the top
-- bit set on all but the last; a number that may be negative is written
-- with its sign in the lowest bit. An instruction is a byte saying which
-- it is and then its operands. "Powder.Bytecode" is the only description
-- of them: the machine in @runtime/@ reads what this writes.
module Powder.Bytecode (image, version) where

import Control.Monad
import Control.Monad.State
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import Data.Char (ord)
import Data.List (delete, elemIndex)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Powder.AxCut
import Powder.Core (ConInfo (..), Export (..), Name, foreignArity, primName)

version :: Int
version = 3

data Emitted = Emitted
  { -- | Each block's instructions, and how many it has.
    blocks :: Map.Map Int (Int, B.Builder),
    nextBlock :: Int,
    strings :: Map.Map String Int,
    -- | How many registers are used.
    registers :: Int
  }

type E = State Emitted

-- | The values that are alive, in AxCut's order, each with the register
-- it is in.
type Env = [(Name, Int)]

-- | A program as an image.
image :: Program -> L.ByteString
image (Program defs entry (given, callback) exports) =
  B.toLazyByteString (B.string7 "PWDR" <> number version <> table <> number (registers final) <> code <> definitions <> number start <> number answer <> exported)
  where
    globals = Map.fromList (zip [x | (x, _, _) <- defs] [0 ..])
    (((starts, start, answer), exported), final) = runState build (Emitted Map.empty 0 Map.empty 1)
    build = do
      bodies <- forM defs (\(_, k, s) -> block globals [k] s)
      begin <- block globals [] entry
      -- The block that answers a call from C, which starts with the
      -- action that answers it in the first register.
      answers <- block globals [given] callback
      -- What C may call: its name, its arguments and result, and the
      -- definition that is the action answering it.
      named <- forM exports $ \(Export name shape result action) -> do
        name' <- string name
        shape' <- string shape
        pure (name' <> shape' <> byte (ord result) <> number (Map.findWithDefault 0 action globals))
      pure ((bodies, begin, answers), number (length exports) <> mconcat named)
    table = number (Map.size (strings final)) <> mconcat [number (L.length encoded) <> B.lazyByteString encoded | text <- byPlace (strings final), let encoded = B.toLazyByteString (B.stringUtf8 text)]
    code = number (Map.size (blocks final)) <> mconcat [number n <> instructions | (n, instructions) <- Map.elems (blocks final)]
    definitions = number (length starts) <> foldMap number starts
    byPlace = Map.elems . Map.fromList . map (\(text, i) -> (i, text)) . Map.toList

-- | A number that is not negative.
number :: (Integral a) => a -> B.Builder
number = count . fromIntegral

count :: Int -> B.Builder
count n
  | n < 128 = B.word8 (fromIntegral n)
  | otherwise = B.word8 (fromIntegral (n .&. 127) .|. 128) <> count (n `shiftR` 7)

-- | A number that may be negative.
signed :: Integer -> B.Builder
signed n = go (if n < 0 then (negate n `shiftL` 1) - 1 else n `shiftL` 1)
  where
    go :: Integer -> B.Builder
    go m
      | m < 128 = B.word8 (fromIntegral m)
      | otherwise = B.word8 (fromIntegral (m .&. 127) .|. 128) <> go (m `shiftR` 7)

byte :: Int -> B.Builder
byte = B.word8 . (fromIntegral :: Int -> Word8)

-- | Some registers: how many, and which.
several :: [Int] -> B.Builder
several rs = number (length rs) <> foldMap number rs

string :: String -> E B.Builder
string text = do
  known <- gets (Map.lookup text . strings)
  case known of
    Just i -> pure (number i)
    Nothing -> do
      i <- gets (Map.size . strings)
      modify (\e -> e {strings = Map.insert text i (strings e)})
      pure (number i)

-- | A register that holds none of some values: the lowest there is. It is
-- noted as one the program uses.
free :: [Int] -> E Int
free taken = do
  let r = length (takeWhile (`elem` taken) [0 ..])
  r <$ modify (\e -> e {registers = max (registers e) (r + 1)})

-- | As many registers as asked for, none of which holds any of some
-- values, and no two the same.
frees :: Int -> [Int] -> E [Int]
frees 0 _ = pure []
frees n taken = do
  r <- free taken
  (r :) <$> frees (n - 1) (r : taken)

-- | Moves to make all at once, each of a register to a register, as moves
-- to make one after another, given a register none of them touches.
-- Where they go round in a circle one value waits in that register.
inTurn :: Int -> [(Int, Int)] -> [(Int, Int)]
inTurn spare = go . filter (uncurry (/=))
  where
    go [] = []
    go moves = case [m | m@(to, _) <- moves, to `notElem` map snd moves] of
      m : _ -> m : go (delete m moves)
      [] -> case moves of
        (to, from) : rest -> (spare, from) : go ((to, spare) : rest)

-- | Emit a statement as a block, given the names of the values it starts
-- with, which are in the first registers in order; and say which block it
-- is.
block :: Map.Map Name Int -> [Name] -> Statement -> E Int
block globals names = blockIn globals (zip names [0 ..])

-- | Emit a statement as a block that starts with values where they are.
blockIn :: Map.Map Name Int -> Env -> Statement -> E Int
blockIn globals env0 statement = do
  me <- state (\e -> (nextBlock e, e {nextBlock = nextBlock e + 1}))
  modify (\e -> e {registers = maximum (registers e : map ((+ 1) . snd) env0)})
  instructions <- go env0 statement
  modify (\e -> e {blocks = Map.insert me (length instructions, mconcat instructions) (blocks e)})
  pure me
  where
    -- The last values of the row, as many as asked, and the rest.
    lastOf :: Int -> Env -> (Env, [Int])
    lastOf n env = let (rest, mine) = splitAt (length env - n) env in (rest, map snd mine)
    methodCode = \case
      Ret -> 0
      Ap -> 1
      Run -> 2
      Forced -> 3
    kindCode = \case
      Closure -> 0
      Thunk -> 1
    -- What an object is, apart from what it captures: its kind, its
    -- method, how many parameters that has, and its block.
    object (Object kind captured (Method m parameters body)) = do
      b <- block globals (parameters ++ captured) body
      pure (byte (kindCode kind) <> byte (methodCode m) <> number (length parameters) <> number b)
    -- A consumer is kept on the stack of frames.
    isFrame (Object kind _ (Method m _ _)) = kind == Closure && m == Ret

    go :: Env -> Statement -> E [B.Builder]
    go env = \case
      -- Each value of the new row is one of the old. The first use of an
      -- old value keeps its register; another use is a copy; and an old
      -- value with no use is dropped.
      Substitute pairs s -> do
        let place old = fromMaybe (error ("Powder.Bytecode: " ++ old ++ " is not in the environment")) (lookup old env)
            assign (done, taken, copies) (new, old)
              | old `elem` map snd done = do
                  r <- free taken
                  pure (done ++ [((new, r), old)], r : taken, copies ++ [byte 1 <> number r <> number (place old)])
              | otherwise = pure (done ++ [((new, place old), old)], taken, copies)
        (done, _, copies) <- foldM assign ([], map snd env, []) pairs
        let drops = [byte 2 <> number r | (old, r) <- env, old `notElem` map snd pairs]
        ((copies ++ drops) ++) <$> go (map fst done) s
      Let v c s -> do
        let (rest, fields) = lastOf (conArity c) env
        to <- free (map snd rest)
        (byte 3 <> number to <> number (conTag c) <> several fields :) <$> go (rest ++ [(v, to)]) s
      Switch clauses -> do
        let (rest, scrutinee) = lastOf 1 env
        targets <- forM clauses $ \(Clause _ xs s) -> do
          fields <- frees (length xs) (map snd rest)
          (,) fields <$> blockIn globals (rest ++ zip xs fields) s
        pure [byte 4 <> foldMap number scrutinee <> number (length targets) <> mconcat [several fields <> number b | (fields, b) <- targets]]
      New v o@(Object _ captured _) s -> do
        let (rest, held) = lastOf (length captured) env
        to <- free (map snd rest)
        how <- object o
        (byte (if isFrame o then 6 else 5) <> number to <> how <> several held :) <$> go (rest ++ [(v, to)]) s
      -- The arguments go to the first registers and the object to the one
      -- after them. Nothing else is alive.
      Invoke m -> do
        let sources = map snd env
        spare <- free (sources ++ [0 .. length sources])
        let moves = inTurn spare (zip [0 ..] sources)
        pure ([byte 0 <> number to <> number from | (to, from) <- moves] ++ [byte 7 <> byte (methodCode m) <> number (length sources - 1)])
      Force -> case env of
        [(_, consumer), (_, value)] -> pure [byte 8 <> number value <> number consumer]
        _ -> error "Powder.Bytecode: a force with more in the environment than a value and its consumer"
      Lit v n s -> do
        to <- free (map snd env)
        if abs n < 2 ^ (62 :: Int)
          then (byte 9 <> number to <> signed n :) <$> go (env ++ [(v, to)]) s
          else do
            text <- string (show n)
            (byte 10 <> number to <> text :) <$> go (env ++ [(v, to)]) s
      Global v g s -> case Map.lookup g globals of
        Just i -> do
          to <- free (map snd env)
          (byte 11 <> number to <> number i :) <$> go (env ++ [(v, to)]) s
        Nothing -> error ("Powder.Bytecode: no definition of " ++ g)
      Prim v p n s -> do
        let (rest, arguments) = lastOf n env
        to <- free (map snd rest)
        name <- string (primName p)
        (byte 12 <> number to <> name <> several arguments :) <$> go (rest ++ [(v, to)]) s
      Foreign v symbol shape result s -> do
        let (rest, arguments) = lastOf (foreignArity symbol shape) env
        to <- free (map snd rest)
        symbol' <- string symbol
        shape' <- string shape
        (byte 13 <> number to <> symbol' <> shape' <> byte (ord result) <> several arguments :) <$> go (rest ++ [(v, to)]) s
      -- Each object captures some of the last values, which are in the
      -- row in the order of the objects, and then some of the objects.
      Rec objects s -> do
        let names = [v | (v, _, _) <- objects]
            owns = [length captured - length others | (_, Object _ captured _, others) <- objects]
            (rest, held) = lastOf (sum owns) env
        tos <- frees (length objects) (map snd rest)
        let shares = scanl (flip drop) held owns
        made <- forM (zip3 objects tos (zip owns shares)) $ \((_, o, others), to, (own, from)) -> do
          how <- object o
          let theirs = map (\x -> fromMaybe (error "Powder.Bytecode: no such object") (elemIndex x names)) others
          pure (number to <> how <> several (take own from) <> several theirs)
        (byte 14 <> number (length made) <> mconcat made :) <$> go (rest ++ zip names tos) s
      Halt -> pure [byte 15]
