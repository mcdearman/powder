-- | Patterns in HIR. Import qualified.
module Powder.HIR.Pat
  ( Pat (..),
    Kind (..),
    binders,
    subpatterns,
  )
where

import Powder.HIR.Id

data Pat = Pat {patId :: NodeId, patKind :: Kind}

data Kind
  = -- | Bind what is matched to a new name.
    Var NameId
  | Wild
  | Lit Literal
  | -- | A constructor and the patterns of its fields. Tuples, lists and the
    -- unit are constructors like any other by now.
    Con NameId [Pat]
  | -- | Bind what is matched to a new name, and match it as well.
    As NameId Pat
  | -- | Matches anything, and is taken apart only when one of its names
    -- is wanted.
    Lazy Pat
  | -- | A constructor pattern whose fields are matched in another order
    -- than the constructor has them: the places of the fields to match
    -- first, in turn. It is what a pattern with field labels is when the
    -- labels are not written in the constructor's order.
    Ordered [Int] Pat
  | -- | Matches what its pattern does, after working out what is matched.
    Bang Pat
  | -- | Where the source was wrong.
    Error

-- | The names a pattern binds, in order.
binders :: Pat -> [NameId]
binders p = case patKind p of
  Var x -> [x]
  As x q -> x : binders q
  Lazy q -> binders q
  Ordered _ q -> binders q
  Bang q -> binders q
  Con _ ps -> concatMap binders ps
  Wild -> []
  Lit _ -> []
  Error -> []

-- | The patterns directly inside a pattern.
subpatterns :: Pat -> [Pat]
subpatterns p = case patKind p of
  Con _ ps -> ps
  As _ q -> [q]
  Lazy q -> [q]
  Ordered _ q -> [q]
  Bang q -> [q]
  _ -> []
