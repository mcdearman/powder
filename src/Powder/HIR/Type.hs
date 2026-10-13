-- | Types as HIR has them: as written, with their names resolved. Import
-- qualified.
--
-- These are still syntax. A list type is the list constructor applied, a
-- tuple type a tuple constructor applied, and a synonym is not yet
-- expanded. The type checker has types of its own, in "Powder.Types".
module Powder.HIR.Type
  ( Type (..),
    Form (..),
  )
where

import Powder.HIR.Id

data Type = Type {typeId :: NodeId, typeForm :: Form}

data Form
  = -- | A type variable.
    Var NameId
  | -- | A type constructor, or a synonym.
    Con NameId
  | App Type Type
  | Fun Type Type
  | -- | A type for all types its variables could be, that are instances
    -- of the classes asked of them: each a class and the types it is
    -- asked of.
    Forall [NameId] [(NameId, [Type])] Type
  | -- | Where the source was wrong.
    Error
