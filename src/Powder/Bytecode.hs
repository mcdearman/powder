-- | Bytecode: AxCut compiled for a machine with registers, written as
-- Glade's is, Meadow's machine.
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
-- Code is in blocks, one after another, each a run of instructions that
-- ends by going somewhere else: the body of a method, or a clause of a
-- switch. A place in the code is the number of an instruction. A method's
-- block starts with its parameters in the first registers and what its
-- object captured in the ones after; a clause's starts with every register
-- as the switch left it, and the fields where the switch was told to put
-- them. Before a method is called its arguments are moved into the first
-- registers, and nothing else is alive: a call does not come back. What
-- comes back is a consumer being called, and a consumer is not an object
-- on the heap but a frame on the machine's stack of them.
--
-- Every instruction is eight bytes: what it is, three registers, and a
-- number of 32 bits.
--
-- >   0        1        2        3        4                7
-- > | opcode |   a    |   b    |   c    |       imm        |
--
-- A register is a byte, so a program has at most 256. What does not fit in
-- an instruction is in tables beside the code, which it names by number:
-- the registers an instruction takes several of, the shapes of objects,
-- the clauses of a switch, numbers too big for 32 bits, primitives and C
-- functions. So a place in the code is an index, the machine reads the
-- code without decoding it, and it can be read a block at a time, as a
-- compiler to machine code would. The instructions:
--
-- > halt                          stop
-- > move    a b                   r[a] = r[b], taken out of r[b]
-- > copy    a b                   r[a] = r[b], and r[b] kept
-- > drop    a                     let go of r[a]
-- > data    a c imm               r[a] = constructor imm of the c registers listed
-- > switch  a imm                 take r[a] apart by switch table imm
-- > closure a c imm               r[a] = an object of shape imm, holding the c listed
-- > thunk   a c imm               r[a] = a thunk of shape imm, holding the c listed
-- > frame   a c imm               r[a] = a consumer of shape imm, on the stack of frames
-- > invoke  b c                   call method b of r[c], the arguments in r[0 .. c]
-- > force   a b                   work out r[a], if it is a thunk, and give it to r[b]
-- > int     a imm                 r[a] = imm, a signed number of 32 bits
-- > const   a imm                 r[a] = constant imm
-- > global  a imm                 r[a] = definition imm
-- > prim    a c imm               r[a] = primitive imm of the c registers listed
-- > prim1   a b imm               r[a] = primitive imm of r[b]
-- > prim2   a b c imm             r[a] = primitive imm of r[b] and r[c]
-- > foreign a c imm               r[a] = C function imm of the c registers listed
-- > rec     imm                   the objects of recursive group imm
--
-- An image is, in order, each count and number four bytes, least first:
--
-- > "PWDR", version, registers
-- > strings      each its length and its bytes: the names of primitives, of C
-- >              functions and of definitions, and big numbers in decimal
-- > constants    each a string
-- > primitives   each a string
-- > foreigns     each its symbol, its shape and its result, as strings and a letter
-- > shapes       each a kind byte, a method byte, two bytes of parameters, a place
-- > lists        a count of bytes, and the registers, a byte each
-- > switches     each its clauses: where its fields' list starts, how many, a place
-- > recs         each its objects: a register, a shape, its own list, the others'
-- > code         each eight bytes
-- > lists at     for each instruction, where the list it takes starts
-- > definitions  each its name and its place
-- > entry, callback, and exports: each a name, a shape, a result and a definition
--
-- "Powder.Bytecode" is the only description of them: the machine in
-- @runtime/@ reads what this writes.
module Powder.Bytecode (image, version) where

import Control.Monad
import Control.Monad.State
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import Data.Char (ord)
import Data.Int (Int32)
import Data.List (delete, elemIndex)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import Powder.AxCut
import Powder.Core (ConInfo (..), Export (..), Name, Prim, foreignArity, primName)

version :: Int
version = 4

-- | An instruction before the code is laid out: what it is, its
-- registers, its number (or the block it is the place of), and the
-- registers it takes several of.
data Instr = Instr Op Int Int Int Imm [Int]

data Imm = Number Int | Place Int

data Op = OHalt | OMove | OCopy | ODrop | OData | OSwitch | OClosure | OThunk | OFrame | OInvoke | OForce | OInt | OConst | OGlobal | OPrim | OPrim1 | OPrim2 | OForeign | ORec
  deriving (Enum)

-- | An object's shape: its kind, its method, how many parameters that
-- has, and the block of its code.
type Shape = (Int, Int, Int, Int)

data Emitted = Emitted
  { -- | Each block's instructions.
    blocks :: Map.Map Int [Instr],
    nextBlock :: Int,
    strings :: Map.Map String Int,
    constants :: [Int],
    primitives :: Map.Map String Int,
    foreigns :: [(Int, Int, Int)],
    shapes :: [Shape],
    -- | Each a clause's fields and its block.
    switches :: [[([Int], Int)]],
    -- | Each an object's register, shape, own registers and the others.
    recs :: [[(Int, Int, [Int], [Int])]],
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
  if registers final > 256
    then error ("Powder.Bytecode: the program needs " ++ show (registers final) ++ " registers, and a machine has 256")
    else B.toLazyByteString (header <> tables <> code <> listsAt <> definitions <> word start <> word answer <> exported)
  where
    globals = Map.fromList (zip [x | (x, _, _) <- defs] [0 ..])
    ((starts, start', answer', exports'), final) = runState build (Emitted Map.empty 0 Map.empty [] Map.empty [] [] [] [] 1)
    build = do
      bodies <- forM defs (\(x, k, s) -> (,) <$> string x <*> block globals [k] s)
      begin <- block globals [] entry
      -- The block that answers a call from C, which starts with the
      -- action that answers it in the first register.
      answers <- block globals [given] callback
      -- What C may call: its name, its arguments and result, and the
      -- definition that is the action answering it.
      named <- forM exports $ \(Export name shape result action) -> do
        name' <- string name
        shape' <- string shape
        pure (word name' <> word shape' <> word (ord result) <> word (Map.findWithDefault 0 action globals))
      pure (bodies, begin, answers, named)
    -- Where each block starts, the blocks one after another.
    places = Map.fromList (zip (Map.keys (blocks final)) (scanl (+) 0 (map length (Map.elems (blocks final)))))
    place b = fromMaybe (error "Powder.Bytecode: no such block") (Map.lookup b places)
    start = place start'
    answer = place answer'
    instructions = concat (Map.elems (blocks final))
    -- The lists, one after another, and where each instruction's starts.
    listOf (Instr _ _ _ _ _ list) = list
    listStarts = scanl (+) 0 (map (length . listOf) instructions)
    allLists = concatMap listOf instructions
    switchLists = [fields | clauses <- reverse (switches final), (fields, _) <- clauses]
    recLists = [own | objects <- reverse (recs final), (_, _, own, _) <- objects]
    pool = allLists ++ concat switchLists ++ concat recLists
    poolStart = length allLists
    switchStarts = scanl (+) poolStart (map length switchLists)
    recStarts = scanl (+) (poolStart + sum (map length switchLists)) (map length recLists)
    header = B.string7 "PWDR" <> word version <> word (registers final)
    tables =
      many (byPlace (strings final)) (\text -> let encoded = B.toLazyByteString (B.stringUtf8 text) in word (fromIntegral (L.length encoded)) <> B.lazyByteString encoded)
        <> many (reverse (constants final)) word
        <> many (byPlace (primitives final)) (\name -> word (Map.findWithDefault 0 name (strings final)))
        <> many (reverse (foreigns final)) (\(s, h, r) -> word s <> word h <> word r)
        <> many (reverse (shapes final)) (\(kind, method, parameters, b) -> B.word8 (fromIntegral kind) <> B.word8 (fromIntegral method) <> B.word16LE (fromIntegral parameters) <> word (place b))
        <> word (length pool)
        <> mconcat (map (B.word8 . fromIntegral) pool)
        <> many (zip (reverse (switches final)) (clauseStarts switchStarts (reverse (switches final)))) (\(clauses, at) -> many (zip clauses at) (\((fields, b), s) -> word s <> word (length fields) <> word (place b)))
        <> many (zip (reverse (recs final)) (clauseStarts recStarts (reverse (recs final)))) (\(objects, at) -> many (zip objects at) (\((r, shape, own, others), s) -> B.word8 (fromIntegral r) <> word shape <> word s <> word (length own) <> many others word))
    clauseStarts :: [Int] -> [[a]] -> [[Int]]
    clauseStarts = go
      where
        go _ [] = []
        go s (g : gs) = let (mine, rest) = splitAt (length g) s in mine : go rest gs
    code = word (length instructions) <> mconcat (map encode instructions)
    encode (Instr op a b c imm _) =
      B.word8 (fromIntegral (fromEnum op)) <> reg a <> reg b <> reg c <> B.word32LE (case imm of Number n -> fromIntegral n; Place p -> fromIntegral (place p))
    reg r = B.word8 (fromIntegral r)
    listsAt = mconcat [word (if null list then 0 else s) | (Instr _ _ _ _ _ list, s) <- zip instructions listStarts]
    definitions = many (zip starts [0 :: Int ..]) (\((name, b), _) -> word name <> word (place b))
    exported = word (length exports') <> mconcat exports'
    byPlace = Map.elems . Map.fromList . map (\(text, i) -> (i, text)) . Map.toList

word :: Int -> B.Builder
word n = B.word32LE (fromIntegral n :: Word32)

many :: [a] -> (a -> B.Builder) -> B.Builder
many xs f = word (length xs) <> foldMap f xs

string :: String -> E Int
string text = do
  known <- gets (Map.lookup text . strings)
  case known of
    Just i -> pure i
    Nothing -> do
      i <- gets (Map.size . strings)
      modify (\e -> e {strings = Map.insert text i (strings e)})
      pure i

primitive :: Prim -> E Int
primitive p = do
  _ <- string (primName p)
  known <- gets (Map.lookup (primName p) . primitives)
  case known of
    Just i -> pure i
    Nothing -> do
      i <- gets (Map.size . primitives)
      modify (\e -> e {primitives = Map.insert (primName p) i (primitives e)})
      pure i

-- | Add to a table, and say where in it.
added :: (Emitted -> [a]) -> (Emitted -> [a] -> Emitted) -> a -> E Int
added get set x = do
  xs <- gets get
  modify (\e -> set e (x : xs))
  pure (length xs)

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
  modify (\e -> e {blocks = Map.insert me instructions (blocks e)})
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
    -- The shape of an object, apart from what it captures.
    shape (Object kind captured (Method m parameters body)) = do
      b <- block globals (parameters ++ captured) body
      added shapes (\e xs -> e {shapes = xs}) (kindCode kind, methodCode m, length parameters, b)
    -- A consumer is kept on the stack of frames.
    isFrame (Object kind _ (Method m _ _)) = kind == Closure && m == Ret
    opOf (Object kind _ _) o
      | isFrame o = OFrame
      | kind == Thunk = OThunk
      | otherwise = OClosure
    instr op a b c imm = Instr op a b c imm []

    go :: Env -> Statement -> E [Instr]
    go env = \case
      -- Each value of the new row is one of the old. The first use of an
      -- old value keeps its register; another use is a copy; and an old
      -- value with no use is dropped.
      Substitute pairs s -> do
        let place old = fromMaybe (error ("Powder.Bytecode: " ++ old ++ " is not in the environment")) (lookup old env)
            assign (done, taken, copies) (new, old)
              | old `elem` map snd done = do
                  r <- free taken
                  pure (done ++ [((new, r), old)], r : taken, copies ++ [instr OCopy r (place old) 0 (Number 0)])
              | otherwise = pure (done ++ [((new, place old), old)], taken, copies)
        (done, _, copies) <- foldM assign ([], map snd env, []) pairs
        let drops = [instr ODrop r 0 0 (Number 0) | (old, r) <- env, old `notElem` map snd pairs]
        ((copies ++ drops) ++) <$> go (map fst done) s
      Let v c s -> do
        let (rest, fields) = lastOf (conArity c) env
        to <- free (map snd rest)
        (Instr OData to 0 (length fields) (Number (conTag c)) fields :) <$> go (rest ++ [(v, to)]) s
      Switch clauses -> do
        let (rest, scrutinee) = lastOf 1 env
        targets <- forM clauses $ \(Clause _ xs s) -> do
          fields <- frees (length xs) (map snd rest)
          (,) fields <$> blockIn globals (rest ++ zip xs fields) s
        table <- added switches (\e xs -> e {switches = xs}) targets
        pure [instr OSwitch r 0 0 (Number table) | r <- take 1 scrutinee]
      New v o@(Object _ captured _) s -> do
        let (rest, held) = lastOf (length captured) env
        to <- free (map snd rest)
        how <- shape o
        (Instr (opOf o o) to 0 (length held) (Number how) held :) <$> go (rest ++ [(v, to)]) s
      -- The arguments go to the first registers and the object to the one
      -- after them. Nothing else is alive.
      Invoke m -> do
        let sources = map snd env
        spare <- free (sources ++ [0 .. length sources])
        let moves = inTurn spare (zip [0 ..] sources)
        pure ([instr OMove to from 0 (Number 0) | (to, from) <- moves] ++ [instr OInvoke 0 (methodCode m) (length sources - 1) (Number 0)])
      Force -> case env of
        [(_, consumer), (_, value)] -> pure [instr OForce value consumer 0 (Number 0)]
        _ -> error "Powder.Bytecode: a force with more in the environment than a value and its consumer"
      Lit v n s -> do
        to <- free (map snd env)
        if n >= toInteger (minBound :: Int32) && n <= toInteger (maxBound :: Int32)
          then (instr OInt to 0 0 (Number (fromInteger n)) :) <$> go (env ++ [(v, to)]) s
          else do
            text <- string (show n)
            i <- added constants (\e xs -> e {constants = xs}) text
            (instr OConst to 0 0 (Number i) :) <$> go (env ++ [(v, to)]) s
      Global v g s -> case Map.lookup g globals of
        Just i -> do
          to <- free (map snd env)
          (instr OGlobal to 0 0 (Number i) :) <$> go (env ++ [(v, to)]) s
        Nothing -> error ("Powder.Bytecode: no definition of " ++ g)
      Prim v p n s -> do
        let (rest, arguments) = lastOf n env
        to <- free (map snd rest)
        i <- primitive p
        let made = case arguments of
              [x] -> instr OPrim1 to x 0 (Number i)
              [x, y] -> instr OPrim2 to x y (Number i)
              _ -> Instr OPrim to 0 (length arguments) (Number i) arguments
        (made :) <$> go (rest ++ [(v, to)]) s
      Foreign v symbol shape' result s -> do
        let (rest, arguments) = lastOf (foreignArity symbol shape') env
        to <- free (map snd rest)
        symbol' <- string symbol
        shape'' <- string shape'
        i <- added foreigns (\e xs -> e {foreigns = xs}) (symbol', shape'', ord result)
        (Instr OForeign to 0 (length arguments) (Number i) arguments :) <$> go (rest ++ [(v, to)]) s
      -- Each object captures some of the last values, which are in the
      -- row in the order of the objects, and then some of the objects.
      Rec objects s -> do
        let names = [v | (v, _, _) <- objects]
            owns = [length captured - length others | (_, Object _ captured _, others) <- objects]
            (rest, held) = lastOf (sum owns) env
        tos <- frees (length objects) (map snd rest)
        let shares = scanl (flip drop) held owns
        made <- forM (zip3 objects tos (zip owns shares)) $ \((_, o, others), to, (own, from)) -> do
          how <- shape o
          let theirs = map (\x -> fromMaybe (error "Powder.Bytecode: no such object") (elemIndex x names)) others
          pure (to, how, take own from, theirs)
        table <- added recs (\e xs -> e {recs = xs}) made
        (instr ORec 0 0 0 (Number table) :) <$> go (rest ++ zip names tos) s
      Halt -> pure [instr OHalt 0 0 0 (Number 0)]
