-- | Bytecode: AxCut written down for a machine to load.
--
-- It is AxCut still, statement for statement, with two things settled
-- that a machine should not have to: a value is named by where it is in
-- the environment, counting from the first, and code is cut into blocks,
-- each a run of instructions that ends by going somewhere else. A clause
-- of a switch and the body of a method are blocks of their own.
--
-- An image is, in order:
--
-- > "PWDR", version
-- > strings      the names of primitives and C functions, and numbers too
-- >              big for sixty-four bits, in decimal
-- > blocks       each its instructions
-- > definitions  each the block that works it out
-- > entry        the block that starts the program
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
import Data.List (elemIndex)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Powder.AxCut
import Powder.Core (ConInfo (..), Export (..), Name, foreignArity, primName)

version :: Int
version = 2

data Emitted = Emitted
  { -- | Each block's instructions, most recent first, and how many it has.
    blocks :: Map.Map Int (Int, B.Builder),
    nextBlock :: Int,
    strings :: Map.Map String Int
  }

type E = State Emitted

-- | A program as an image.
image :: Program -> L.ByteString
image (Program defs entry (given, callback) exports) = B.toLazyByteString (B.string7 "PWDR" <> number version <> table <> code <> definitions <> number start <> number answer <> exported)
  where
    globals = Map.fromList (zip [x | (x, _, _) <- defs] [0 ..])
    (((starts, start, answer), exported), final) = runState build (Emitted Map.empty 0 Map.empty)
    build = do
      bodies <- forM defs (\(_, k, s) -> block globals [k] s)
      begin <- block globals [] entry
      -- The block that answers a call from C, which starts with the
      -- action that answers it in the environment.
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

string :: String -> E B.Builder
string text = do
  known <- gets (Map.lookup text . strings)
  case known of
    Just i -> pure (number i)
    Nothing -> do
      i <- gets (Map.size . strings)
      modify (\e -> e {strings = Map.insert text i (strings e)})
      pure (number i)

-- | Emit a statement as a block, given the names of the values in its
-- environment, and say which block it is.
block :: Map.Map Name Int -> [Name] -> Statement -> E Int
block globals env0 statement = do
  me <- state (\e -> (nextBlock e, e {nextBlock = nextBlock e + 1}))
  instructions <- go env0 statement
  modify (\e -> e {blocks = Map.insert me (length instructions, mconcat instructions) (blocks e)})
  pure me
  where
    without n env = take (length env - n) env
    methodCode = \case
      Ret -> 0
      Ap -> 1
      Run -> 2
      Forced -> 3
    kindCode = \case
      Closure -> 0
      Thunk -> 1
    -- What an object is, apart from what it captures.
    object (Object kind captured (Method m parameters body)) = do
      b <- block globals (parameters ++ captured) body
      pure (byte (kindCode kind) <> byte (methodCode m), number (length parameters) <> number b)

    go :: [Name] -> Statement -> E [B.Builder]
    go env = \case
      Substitute pairs s -> do
        let place old = fromMaybe (error ("Powder.Bytecode: " ++ old ++ " is not in the environment")) (elemIndex old env)
        (byte 0 <> number (length pairs) <> foldMap (number . place . snd) pairs :) <$> go (map fst pairs) s
      Let v c s -> (byte 1 <> number (conTag c) <> number (conArity c) :) <$> go (without (conArity c) env ++ [v]) s
      Switch clauses -> do
        targets <- forM clauses (\(Clause _ xs s) -> (,) (length xs) <$> block globals (without 1 env ++ xs) s)
        pure [byte 2 <> number (length targets) <> mconcat [number n <> number b | (n, b) <- targets]]
      New v o@(Object _ captured _) s -> do
        (what, how) <- object o
        (byte 3 <> what <> number (length captured) <> how :) <$> go (without (length captured) env ++ [v]) s
      Invoke m -> pure [byte 4 <> byte (methodCode m)]
      Force -> pure [byte 5]
      Lit v n s
        | abs n < 2 ^ (62 :: Int) -> (byte 6 <> signed n :) <$> go (env ++ [v]) s
        | otherwise -> do
            text <- string (show n)
            (byte 7 <> text :) <$> go (env ++ [v]) s
      Global v g s -> case Map.lookup g globals of
        Just i -> (byte 8 <> number i :) <$> go (env ++ [v]) s
        Nothing -> error ("Powder.Bytecode: no definition of " ++ g)
      Prim v p n s -> do
        name <- string (primName p)
        (byte 9 <> name <> number n :) <$> go (without n env ++ [v]) s
      Foreign v symbol shape result s -> do
        symbol' <- string symbol
        shape' <- string shape
        (byte 10 <> symbol' <> shape' <> byte (ord result) :) <$> go (without (foreignArity symbol shape) env ++ [v]) s
      Rec objects s -> do
        let names = [v | (v, _, _) <- objects]
        made <- forM objects $ \(_, o@(Object _ captured _), others) -> do
          (what, how) <- object o
          let theirs = map (\x -> fromMaybe (error "Powder.Bytecode: no such object") (elemIndex x names)) others
          pure (length captured - length others, what <> number (length captured - length others) <> number (length others) <> foldMap number theirs <> how)
        (byte 11 <> number (length made) <> foldMap snd made :) <$> go (without (sum (map fst made)) env ++ names) s
      Halt -> pure [byte 12]
