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
    freeVars,
    shake,
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
    progDefs :: [(Name, Core)]
  }

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
shake :: Name -> Program -> Program
shake root (Program cons defs) = Program cons [d | d@(x, _) <- defs, x `Set.member` live]
  where
    env = Map.fromList defs
    live = go Set.empty [root]
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
