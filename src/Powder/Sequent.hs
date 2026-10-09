-- | The sequent calculus: the backend's upper IR, after Core.
--
-- It is the λμμ̃ calculus, call by value and with no types. A program is
-- made of /producers/, which make values; /consumers/, which use them
-- up; and /statements/, which are what runs. The one way to compute is a
-- cut, a producer set against a consumer.
--
-- Three kinds of thing are defined by how they are used, each by one way
-- of using it: a function is something to apply, to an argument and to
-- the consumer of its result; an action is something to run, given a
-- consumer for its result and one for what it raises; and a thunk is
-- something to force.
--
-- Haskell is lazy and this calculus is not, so laziness is written out. A
-- variable stands for something that may not have been worked out yet: a
-- thunk, or a value. To look at it, it is forced, which works a thunk
-- out, once, and does nothing to a value. Every thunk made and every
-- force is there in the program to be seen, which is the point: a pass
-- that finds a thunk is always forced, or cheap to work out at once, can
-- take it away.
module Powder.Sequent
  ( Name,
    Producer (..),
    Consumer (..),
    Alt (..),
    Statement (..),
    Program (..),
    raised,
    freeNames,
    pretty,
  )
where

import Data.List (intercalate)
import qualified Data.Set as Set
import Powder.Core (ConInfo (..), Name, Prim, primName)

data Producer
  = -- | A variable: a thunk, or a value.
    Var Name
  | -- | A definition at the top of the program, which is a thunk.
    Global Name
  | Lit Integer
  | -- | A constructor and its fields, which are not forced.
    Con ConInfo [Producer]
  | -- | @μα. s@: run the statement, with α naming whatever consumes this.
    Mu Name Statement
  | -- | A function: its argument, the consumer of its result, its body.
    Fun Name Name Statement
  | -- | Something not yet worked out. Forcing it runs the statement, whose
    -- consumer is given the value; the value is kept, so it runs once.
    Thunk Name Statement
  | -- | An action: the consumer of its result, the consumer of what it
    -- raises, and what running it does.
    Action Name Name Statement

data Consumer
  = Covar Name
  | -- | @μ̃x. s@: name the value and go on.
    MuTilde Name Statement
  | -- | Take a constructed value apart. The alternatives are one for each
    -- constructor of its type, in order.
    Case [Alt]
  | -- | Apply a function to an argument, the result going to a consumer.
    Ap Producer Consumer
  | -- | Work a thunk out, the value going to a consumer. A value goes
    -- straight there.
    Force Consumer
  | -- | Run an action, given where its result goes and where what it
    -- raises goes.
    Run Consumer Consumer

data Alt = Alt ConInfo [Name] Statement

data Statement
  = Cut Producer Consumer
  | -- | A primitive of values, each already worked out, with where its
    -- result goes.
    Prim Prim [Producer] Consumer
  | -- | A C function, as in Core, of values already worked out.
    Foreign Name String Char [Producer] Consumer
  | -- | Things that refer to each other, each a function, a thunk, an
    -- action or a constructor, and then a statement in their scope.
    LetRec [(Name, Producer)] Statement
  | -- | The program is over.
    Halt

-- | A program: its definitions, each a thunk, and the statement that
-- starts it.
data Program = Program
  { programDefinitions :: [(Name, Producer)],
    programEntry :: Statement
  }

-- | What an action raises: what to do if nothing catches it, and what was
-- raised.
raised :: ConInfo
raised = ConInfo "%Raised" 0 2 1

-- | The variables and covariables a statement uses and does not bind.
-- Definitions at the top are not among them.
freeNames :: Statement -> Set.Set Name
freeNames = statement
  where
    statement = \case
      Cut p c -> producer p <> consumer c
      Prim _ ps c -> foldMap producer ps <> consumer c
      Foreign _ _ _ ps c -> foldMap producer ps <> consumer c
      LetRec bs s -> (foldMap (producer . snd) bs <> statement s) Set.\\ Set.fromList (map fst bs)
      Halt -> Set.empty
    producer = \case
      Var x -> Set.singleton x
      Global _ -> Set.empty
      Lit _ -> Set.empty
      Con _ ps -> foldMap producer ps
      Mu a s -> Set.delete a (statement s)
      Fun x a s -> statement s Set.\\ Set.fromList [x, a]
      Thunk a s -> Set.delete a (statement s)
      Action a b s -> statement s Set.\\ Set.fromList [a, b]
    consumer = \case
      Covar a -> Set.singleton a
      MuTilde x s -> Set.delete x (statement s)
      Case alts -> mconcat [statement s Set.\\ Set.fromList xs | Alt _ xs s <- alts]
      Ap p c -> producer p <> consumer c
      Force c -> consumer c
      Run a b -> consumer a <> consumer b

-- Pretty printing -----------------------------------------------------------

pretty :: Program -> String
pretty (Program defs entry) = unlines ([x ++ " = " ++ producer 1 p | (x, p) <- defs] ++ ["entry = " ++ statement 1 entry])
  where
    indent n = "\n" ++ replicate (2 * n) ' '
    statement :: Int -> Statement -> String
    statement n = \case
      Cut p c -> "<" ++ producer n p ++ " | " ++ consumer n c ++ ">"
      Prim p ps c -> primName p ++ "(" ++ intercalate ", " (map (producer n) ps) ++ "; " ++ consumer n c ++ ")"
      Foreign symbol _ _ ps c -> "foreign:" ++ symbol ++ "(" ++ intercalate ", " (map (producer n) ps) ++ "; " ++ consumer n c ++ ")"
      LetRec bs s -> "letrec {" ++ concat [indent (n + 1) ++ x ++ " = " ++ producer (n + 2) p | (x, p) <- bs] ++ indent n ++ "} " ++ statement n s
      Halt -> "halt"
    producer :: Int -> Producer -> String
    producer n = \case
      Var x -> x
      Global g -> "@" ++ g
      Lit i -> show i
      Con c [] -> conName c
      Con c ps -> conName c ++ "(" ++ intercalate ", " (map (producer n) ps) ++ ")"
      Mu a s -> "mu " ++ a ++ "." ++ indent (n + 1) ++ statement (n + 1) s
      Fun x a s -> "fun(" ++ x ++ "; " ++ a ++ ")." ++ indent (n + 1) ++ statement (n + 1) s
      Thunk a s -> "thunk(" ++ a ++ ")." ++ indent (n + 1) ++ statement (n + 1) s
      Action a b s -> "action(" ++ a ++ "; " ++ b ++ ")." ++ indent (n + 1) ++ statement (n + 1) s
    consumer :: Int -> Consumer -> String
    consumer n = \case
      Covar a -> a
      MuTilde x s -> "mu~ " ++ x ++ "." ++ indent (n + 1) ++ statement (n + 1) s
      Case alts -> "case {" ++ concat [indent (n + 1) ++ unwords (conName c : xs) ++ " => " ++ statement (n + 2) s | Alt c xs s <- alts] ++ indent n ++ "}"
      Ap p c -> "ap(" ++ producer n p ++ "; " ++ consumer n c ++ ")"
      Force c -> "force(" ++ consumer n c ++ ")"
      Run a b -> "run(" ++ consumer n a ++ "; " ++ consumer n b ++ ")"
