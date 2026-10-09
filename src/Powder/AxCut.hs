-- | AxCut: the backend's lower IR, which a machine can run as it stands.
--
-- It is the sequent calculus with every cut reduced until a variable
-- stands on one side of it, after Schuster, Müller, Ostermann and
-- Brachthäuser, "Compiling Classical Sequent Calculus to Stock Hardware"
-- (OOPSLA 2025). There are no expressions and no cut: there are
-- statements, each a cut with the rule it meets folded in.
--
-- There is one environment, and it is a row of values with no names in
-- it. A statement says which end of the row it works on, and what it
-- leaves there:
--
-- > let v = K(n)       the last n values become one, a constructed value
-- > switch             the last value is taken apart; its fields take its place
-- > new v = (n){...}   the last n values are captured in an object
-- > invoke m           the last value is an object; the rest are the arguments
-- >                    of its method, and what it captured takes its place
-- > substitute         the row is made again from itself: values moved,
-- >                    copied and dropped
--
-- So nothing in a statement names where a value is: the names here are
-- for reading, and for checking that the lowering got the row right. And
-- the environment is linear: a value is used once, and to use it twice
-- or not at all is a @substitute@ saying so. That is what lets a machine
-- count references by the statements alone, and free what it drops.
--
-- To the paper's statements are added what a lazy language needs. A
-- /thunk/ is an object of one method, to force it, that the machine
-- replaces with its value; @force@ works the last value out if it is
-- one. And @rec@ makes objects that capture each other.
module Powder.AxCut
  ( Program (..),
    Statement (..),
    Clause (..),
    Method (..),
    MethodName (..),
    Kind (..),
    Object (..),
    pretty,
  )
where

import Data.List (intercalate)
import Powder.Core (ConInfo (..), Name, Prim, primName)

-- | A program: its definitions, each the body of a thunk, to be run with
-- only the consumer of its value in the environment; and the statement
-- that starts it, with nothing in the environment.
data Program = Program
  { programDefinitions :: [(Name, Name, Statement)],
    programEntry :: Statement
  }

data Statement
  = -- | Make the environment again: each value of the new one, with the
    -- value of the old it is.
    Substitute [(Name, Name)] Statement
  | -- | Construct, from the last values, as many as the constructor has
    -- fields.
    Let Name ConInfo Statement
  | -- | Take the last value apart. One clause for each constructor of its
    -- type, in order.
    Switch [Clause]
  | -- | Make an object that captures the last values, named here as its
    -- methods know them.
    New Name Object Statement
  | -- | Call a method of the last value, which is an object.
    Invoke MethodName
  | -- | The last value is something that may be a thunk, and the one
    -- before it the consumer of what it is. Work it out and give it over.
    Force
  | Lit Name Integer Statement
  | -- | A definition at the top.
    Global Name Name Statement
  | -- | A primitive of the last values, as many as it takes.
    Prim Name Prim Int Statement
  | -- | A C function of the last values.
    Foreign Name Name String Char Statement
  | -- | Objects that may capture each other. Each captures some of the
    -- last values, which are in the environment in the order of the
    -- objects, and then some of the objects themselves.
    Rec [(Name, Object, [Name])] Statement
  | Halt

-- | The fields a clause names, and what to do with them.
data Clause = Clause ConInfo [Name] Statement

data Kind = Closure | Thunk
  deriving (Eq, Show)

-- | An object: what it captures, and its method. The method runs with
-- its parameters and then what was captured.
data Object = Object Kind [Name] Method

data Method = Method MethodName [Name] Statement

data MethodName
  = -- | Of a consumer: take a value.
    Ret
  | -- | Of a function: take an argument and the consumer of the result.
    Ap
  | -- | Of an action: take the consumers of its result and of what it raises.
    Run
  | -- | Of a thunk: take the consumer of its value.
    Forced
  deriving (Eq, Show)

-- Pretty printing -----------------------------------------------------------

pretty :: Program -> String
pretty (Program defs entry) = unlines (["def " ++ x ++ "(" ++ k ++ ") =" ++ statement 1 s | (x, k, s) <- defs] ++ ["entry =" ++ statement 1 entry])
  where
    indent n = "\n" ++ replicate (2 * n) ' '
    statement :: Int -> Statement -> String
    statement n = \case
      Substitute pairs s -> indent n ++ "substitute [" ++ intercalate ", " [if new == old then new else new ++ " := " ++ old | (new, old) <- pairs] ++ "]" ++ statement n s
      Let v c s -> indent n ++ "let " ++ v ++ " = " ++ conName c ++ "/" ++ show (conArity c) ++ statement n s
      Switch clauses -> indent n ++ "switch {" ++ concat [indent (n + 1) ++ unwords (conName c : xs) ++ " =>" ++ statement (n + 2) s | Clause c xs s <- clauses] ++ indent n ++ "}"
      New v o s -> indent n ++ "new " ++ v ++ " = " ++ object n o ++ statement n s
      Invoke m -> indent n ++ "invoke " ++ show m
      Force -> indent n ++ "force"
      Lit v i s -> indent n ++ "lit " ++ v ++ " = " ++ show i ++ statement n s
      Global v g s -> indent n ++ "global " ++ v ++ " = @" ++ g ++ statement n s
      Prim v p k s -> indent n ++ "prim " ++ v ++ " = " ++ primName p ++ "/" ++ show k ++ statement n s
      Foreign v symbol shape _ s -> indent n ++ "foreign " ++ v ++ " = " ++ symbol ++ "/" ++ show (length shape) ++ statement n s
      Rec os s -> indent n ++ "rec {" ++ concat [indent (n + 1) ++ v ++ " = " ++ object (n + 1) o ++ " with [" ++ unwords others ++ "]" | (v, o, others) <- os] ++ indent n ++ "}" ++ statement n s
      Halt -> indent n ++ "halt"
    object n (Object kind captured (Method m parameters s)) =
      show kind ++ "(" ++ unwords captured ++ "){ " ++ show m ++ "(" ++ unwords parameters ++ ") =>" ++ statement (n + 2) s ++ " }"
