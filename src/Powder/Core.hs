-- | Core: the small lambda calculus every backend compiles from.
module Powder.Core
  ( Name,
    Prim (..),
    primName,
    ConInfo (..),
    falseCon,
    trueCon,
    Core (..),
    Bind (..),
    Alt (..),
    Program (..),
    Export (..),
    foreignArity,
    freeVars,
    shake,
    started,
    prettyDef,
  )
where

import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Primitive operations each backend must provide. Source code refers to
-- them as @prim@ followed by the constructor name, e.g. @primIntAdd@.
--
-- Integers are the only primitive data; characters are their code points.
-- Comparisons return the constructors of @Bool@.
type Name = String

data Prim
  = IntAdd
  | IntSub
  | IntMul
  | IntQuot
  | IntRem
  | IntEq
  | IntLt
  | IntLe
  | -- | @seq a b@: evaluate @a@ to weak head normal form, return @b@.
    Seq
  | -- | Evaluating this is a pattern match failure.
    MatchFail
  | -- | Run an IO action as a side effect of evaluation, giving its result.
    PerformIO
  | Return
  | Bind
  | -- | @putChar handle char@, where handle 1 is stdout and 2 is stderr.
    PutChar
  | -- | Read a character from stdin; -1 at end of input.
    GetChar
  | -- | Exit with the given status.
    Exit
  | -- Files. A path is given a character at a time and then opened, for
    -- reading (0), writing (1) or adding to (2), which gives a handle to
    -- read from, write to with 'PutChar', and close; or -1.
    PathChar
  | -- | Raise something, given what to do if nothing catches it; and do
    -- an action, with what to do with anything raised in it.
    Throw
  | Catch
  | FileOpen
  | HGetChar
  | FileClose
  | -- | Whether a handle has nothing more to read.
    HIsEOF
  | -- | How many arguments the program was given; and the code of the
    -- character at a place in one of them, or in the program's name for
    -- argument -1, or -1 past its end.
    ArgCount
  | ArgChar
  | -- | Move to a place in a file and give the place: counted from the
    -- start (0), from where it is (1) or from the end (2); or give its
    -- size (3). Make a file a size. Send on what has been written. All
    -- give -1 where it cannot be done.
    FileSeek
  | FileTruncate
  | FileFlush
  | -- | Whether a handle is a terminal.
    IsTerminal
  | -- | The code of a character of the environment: of the entry at a
    -- place, written @NAME=value@, the character at a place; or -1.
    EnvChar
  | -- | A cell: a place for a value that can be changed. Make one, giving
    -- its number; and read and write the one with a number.
    CellNew
  | CellRead
  | CellWrite
  | -- | Memory, as C sees it. What is at an address is said by a number:
    -- 0 to 3 for the signed integers of 8, 16, 32 and 64 bits, 4 to 7
    -- for the unsigned ones, 8 for a float and 9 for a double. Each takes
    -- the number, then the address, and a poke then the value.
    PeekInt
  | PeekDouble
  | PokeInt
  | PokeDouble
  | -- | What C's @errno@ was after the last call of a C function, and
    -- setting what the next one starts with.
    ErrnoGet
  | ErrnoSet
  | -- | For an action answering a call from C: the argument at a place,
    -- as an integer or as a double, and giving the result, as one or the
    -- other. They are of the call being answered, which is the innermost
    -- if calls are inside each other.
    CallbackInt
  | CallbackDouble
  | CallbackReturnInt
  | CallbackReturnDouble
  | -- | A character's code, and the character with a code. Both are the
    -- identity when a program runs: a character is its code.
    CharToInt
  | IntToChar
  | -- | Anything as anything else: the identity, at a type that says
    -- otherwise. For giving one representation two types.
    Coerce
  | -- | An integer as the 64-bit integer it is the same as, counting round;
    -- and a double as the nearest single-precision number.
    IntNarrow
  | FloatNarrow
  | -- | As 'DoubleShowAt', for a double shown as a single-precision number.
    FloatShowAt
  | -- | Of a character's code: which of Unicode's thirty general
    -- categories it is in, in their order; and the codes of its upper
    -- case, lower case and title case.
    CharCategory
  | CharUpper
  | CharLower
  | CharTitle
  | -- Floating point. A function of two arguments that is named for one
    -- thing takes that last.
    IntToDouble
  | DoubleAdd
  | DoubleSub
  | DoubleMul
  | DoubleDiv
  | DoublePow
  | DoubleAtan2
  | DoubleEq
  | DoubleLt
  | DoubleLe
  | -- | The integer nearest that is no further from zero.
    DoubleTruncate
  | -- | One of the functions of mathematics, by its number, of a double.
    DoubleFun
  | -- | The code of the character at a place in a double as it is shown,
    -- or -1 past its end.
    DoubleShowAt
  | -- | The two integers a double is made of: it is the first times two to
    -- the second. And a double made of two.
    DoubleMantissa
  | DoubleExponent
  | DoubleEncode
  | -- | The double nearest the ratio of two integers.
    DoubleFromRatio
  | -- | Integers a bit at a time, as two's complement without end: where
    -- both have a bit, where either has, where one has; and an integer
    -- with its bits moved up or down some places, the sign kept.
    IntAnd
  | IntOr
  | IntXor
  | IntShiftL
  | IntShiftR
  | -- | Of the low sixty-four bits of an integer: how many are set; the
    -- place of the lowest that is, or 64; and of the highest, or -1.
    BitsCount
  | BitsLowest
  | BitsHighest
  | -- | Bytes side by side, of a number fixed when they are made. Make
    -- some, all nothing; write one at a place; and copy a run of them
    -- from one to another: to, at, from, at, how many. These are actions.
    BytesNew
  | BytesWrite
  | BytesCopy
  | -- | The byte at a place, and how many there are; and two runs
    -- compared a byte at a time, giving -1, 0 or 1: one, at, the other,
    -- at, how many. These are not actions, and are for bytes that are no
    -- longer written.
    BytesRead
  | BytesLength
  | BytesCompare
  deriving (Eq, Ord, Show, Enum, Bounded)

primName :: Prim -> Name
primName p = "prim" ++ show p

data ConInfo = ConInfo
  { conName :: Name,
    -- | Position among the constructors of its type, from 0.
    conTag :: Int,
    conArity :: Int,
    -- | How many constructors its type has.
    conSiblings :: Int
  }
  deriving (Show)

falseCon, trueCon :: ConInfo
falseCon = ConInfo "False" 0 0 2
trueCon = ConInfo "True" 1 0 2

data Core
  = Var Name
  | Lit Integer
  | Prim Prim
  | Con ConInfo
  | App Core Core
  | Lam Name Core
  | -- | A C function: what C calls it, what its arguments are, each a
    -- double (@D@) or an integer (@L@), and what it gives, by a letter
    -- the backends know.
    Foreign Name String Char
  | Let Bind Core
  | -- | Alternatives are exhaustive and in tag order: exactly one per
    -- constructor of the scrutinee's type.
    Case Core [Alt]

data Bind
  = NonRec Name Core
  | -- | A minimal group of mutually recursive bindings.
    Rec [(Name, Core)]

data Alt = Alt ConInfo [Name] Core

-- | Top-level definitions, all in scope in each other, with entry point
-- @main@.
data Program = Program
  { progCons :: [ConInfo],
    progDefs :: [(Name, Core)],
    -- | The functions C may call.
    progExports :: [Export]
  }

-- | A function C may call: what C calls it; what its arguments are and
-- what it gives, by the letters of 'Foreign'; and the definition that is
-- the action answering a call. The action gets the arguments and gives
-- the result by the primitives for that.
data Export = Export {exportName :: String, exportShape :: String, exportResult :: Char, exportAction :: Name}

freeVars :: Core -> Set.Set Name
freeVars = \case
  Var x -> Set.singleton x
  Lit _ -> Set.empty
  Prim _ -> Set.empty
  Con _ -> Set.empty
  Foreign {} -> Set.empty
  App f a -> freeVars f <> freeVars a
  Lam x e -> Set.delete x (freeVars e)
  Let (NonRec x e) body -> freeVars e <> Set.delete x (freeVars body)
  Let (Rec bs) body -> foldMap freeVars (body : map snd bs) Set.\\ Set.fromList (map fst bs)
  Case scrut alts -> freeVars scrut <> mconcat [freeVars e Set.\\ Set.fromList xs | Alt _ xs e <- alts]

-- | Drop the definitions the given root does not reach.
-- | How many arguments a C function is given here. One made from a
-- function of the program's is given that function, as the action that
-- answers a call, whatever the function's own arguments are.
foreignArity :: Name -> String -> Int
foreignArity symbol shape = if symbol == "wrapper" then 1 else length shape

-- | A program whose root is run by way of another definition, which is
-- given it: the root is the other applied to what the root was. Nothing
-- is changed if either is not there.
started :: Name -> Name -> Program -> Program
started root by (Program cons defs exports)
  | all (`elem` map fst defs) [root, by] = Program cons ((root, App (Var by) (Var inner)) : [(if x == root then inner else x, renamed e) | (x, e) <- defs]) exports
  | otherwise = Program cons defs exports
  where
    inner = root ++ "#"
    -- What a program defines at the top is never the name of anything
    -- inside a definition, so nothing can be in the way.
    renamed = \case
      Var x -> Var (if x == root then inner else x)
      App f a -> App (renamed f) (renamed a)
      Lam x e -> Lam x (renamed e)
      Let (NonRec x e) body -> Let (NonRec x (renamed e)) (renamed body)
      Let (Rec bs) body -> Let (Rec [(x, renamed e) | (x, e) <- bs]) (renamed body)
      Case e alts -> Case (renamed e) [Alt c xs (renamed body) | Alt c xs body <- alts]
      e -> e

shake :: Name -> Program -> Program
shake root (Program cons defs exports) = Program cons [d | d@(x, _) <- defs, x `Set.member` live] exports
  where
    env = Map.fromList defs
    -- What C may call is wanted whatever the program does with it.
    live = go Set.empty (root : map exportAction exports)
    go seen [] = seen
    go seen (x : xs) = case Map.lookup x env of
      Just e | x `Set.notMember` seen -> go (Set.insert x seen) (Set.toList (freeVars e) ++ xs)
      _ -> go seen xs

-- Pretty printing ---------------------------------------------------------

prettyDef :: (Name, Core) -> String
prettyDef (x, e) = ppName x ++ " = " ++ ppCore 0 e ""

-- Operators are shown in parentheses.
ppName :: Name -> String
ppName x
  | all (`elem` "!#$%&*+./<=>?@\\^|-~:") x = "(" ++ x ++ ")"
  | otherwise = x

ppCore :: Int -> Core -> ShowS
ppCore p = \case
  Var x -> showString (ppName x)
  Lit n -> showsPrec (if p > 1 then 11 else 0) n
  Prim q -> showString (primName q)
  Foreign symbol _ _ -> showString ("foreign:" ++ symbol)
  Con c -> showString (ppName (conName c))
  App f a -> showParen (p > 1) (ppCore 1 f . showChar ' ' . ppCore 2 a)
  Lam x e -> showParen (p > 0) (showString ("\\" ++ x ++ " -> ") . ppCore 0 e)
  Let b e -> showParen (p > 0) (showString "let " . binds b . showString " in " . ppCore 0 e)
  Case scrut alts ->
    showParen (p > 0) $
      showString "case " . ppCore 0 scrut . showString " of { " . sepBy "; " (map alt alts) . showString " }"
  where
    binds (NonRec x e) = bind (x, e)
    binds (Rec bs) = showString "rec { " . sepBy "; " (map bind bs) . showString " }"
    bind (x, e) = showString (ppName x ++ " = ") . ppCore 0 e
    alt (Alt c xs e) = showString (unwords (ppName (conName c) : xs) ++ " -> ") . ppCore 0 e
    sepBy s = foldr (.) id . punctuate (showString s)
    punctuate s (x : xs@(_ : _)) = x : s : punctuate s xs
    punctuate _ xs = xs
