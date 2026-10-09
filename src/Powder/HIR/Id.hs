-- | What HIR is keyed by.
module Powder.HIR.Id
  ( NodeId (..),
    NameId (..),
    Table,
    Literal (..),
  )
where

import qualified Data.IntMap.Strict as IntMap

-- | Every node of HIR has an identity of its own: each expression, pattern,
-- binding, clause, right-hand side and alternative. No two nodes of a
-- program share one, however alike they are. Whatever a later pass works
-- out about a node, it keeps in a table under this.
newtype NodeId = NodeId {nodeKey :: Int}
  deriving (Eq, Ord, Show)

-- | Every name a program binds has an identity of its own: each top-level
-- definition, each constructor, and each variable a pattern or a local
-- definition brings into scope. A use of a name is the identity of what it
-- refers to, so scope is settled: two variables spelled the same in
-- different scopes are different names, and a name is the same name
-- everywhere it is used.
newtype NameId = NameId {nameKey :: Int}
  deriving (Eq, Ord, Show)

-- | A side table: something known about some nodes, or some names, kept
-- apart from the tree under their keys.
type Table = IntMap.IntMap

data Literal
  = Number Integer
  | -- | A number written with a fraction or an exponent, exactly.
    Fraction Rational
  | Character Char
  | Text String
  deriving (Eq, Show)
