{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Compiling a package: the modules of one, taken together, after the
-- packages it depends on and without their sources.
--
-- The package is the unit of compilation. Its modules may import each
-- other in a circle, and are resolved, checked and lowered as one. What
-- comes of it is a 'Compiled': the package's code, in Core, and an
-- 'Interface', which is everything a package compiled later needs to
-- know of this one and nothing of how it is made: what its modules
-- export, its types, classes and instances, and the type of everything
-- it defines. A package is compiled given the interfaces of all the
-- packages below it, and its own interface says only what it adds, so
-- that each can be kept in a file and the files put together.
--
-- Packages compiled apart must not make the same identities, so each is
-- given a slot, and makes its names and nodes in the 'chunk' of numbers
-- that is its slot's. The slot is the build tool's to choose; two
-- packages of one program must not share one.
--
-- A program is the code of its packages put together: 'link'.
module Powder.Package
  ( Compiled (..),
    Interface,
    compile,
    link,
    entry,
  )
where

import Control.Monad
import Data.Bifunctor (first)
import Data.Binary (Binary)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Powder.Core (Alt (..), Bind (..), ConInfo (..), Core (..), Name, Prim)
import qualified Powder.Core as Core
import Powder.Diagnostics (Failure (..))
import Powder.Frontend (Unit (..), unitModule)
import Powder.HIR
  ( Binding (..),
    BindingKind (..),
    Builtins (..),
    Class (..),
    Constraint (..),
    Overlap (..),
    DataType (..),
    Instance (..),
    NameId (..),
    NameInfo (..),
    NameSort (..),
    NodeId (..),
    Origin (..),
    Signature (..),
    Synonym (..),
    Table,
    chunk,
  )
import qualified Powder.HIR as HIR
import qualified Powder.HIR.Type as HIR.Type
import Powder.Kinds (kindcheck)
import Powder.Lower (lower)
import Powder.Resolve (Linkage (..), Scope (..), Seed (..), resolveAfter)
import Powder.Syntax.AST (Module)
import Powder.Syntax.AST.Decl (Assoc (..))
import Powder.Syntax.Token (Pos (..))
import Powder.Typecheck (Checked (..), typecheckAfter)
import Powder.Types (Pred (..), Scheme (..))
import qualified Powder.Types as Types

-- | A package, compiled.
data Compiled = Compiled
  { compiledInterface :: Interface,
    compiledCode :: Core.Program
  }
  deriving (Generic, Binary)

-- | What a package adds to what is known: the part of a program's tables
-- that is its own, what its modules export, and the types of what it
-- defines. Interfaces of different packages have nothing in common, and
-- are put together with '<>'.
data Interface = Interface
  { ifaceLinkage :: Linkage,
    ifaceNames :: Table NameInfo,
    -- | Where each instance is, which says what module it is in: an
    -- instance is seen only from where its module is.
    ifaceOrigins :: Table Origin,
    ifaceSignatures :: Table Signature,
    ifaceSynonyms :: Table Synonym,
    ifaceDataTypes :: [DataType],
    ifaceClasses :: [ClassShape],
    ifaceInstances :: [InstanceShape],
    ifaceNewtypes :: [NameId],
    ifaceModules :: [(FilePath, String)],
    ifaceImports :: [(String, [String])],
    ifaceSchemes :: Table Scheme
  }
  deriving (Generic, Binary)

instance Semigroup Interface where
  Interface a b c d e f g h i j k l <> Interface a' b' c' d' e' f' g' h' i' j' k' l' =
    Interface (a <> a') (IntMap.union b b') (IntMap.union c c') (IntMap.union d d') (IntMap.union e e') (f ++ f') (g ++ g') (h ++ h') (i ++ i') (j ++ j') (k ++ k') (IntMap.union l l')

instance Monoid Interface where
  mempty = Interface mempty IntMap.empty IntMap.empty IntMap.empty IntMap.empty [] [] [] [] [] [] IntMap.empty

-- | A class without its code: its name, variables, superclasses,
-- dependencies and methods, and which methods have a definition by
-- default.
data ClassShape = ClassShape NameId [NameId] [Constraint] [([Int], [Int])] [NameId] [NameId]
  deriving (Generic, Binary)

-- | An instance without its code.
data InstanceShape = InstanceShape NodeId NameId [HIR.Type.Type] [NameId] [Constraint] Int
  deriving (Generic, Binary)

-- | What an instance says of others that fit the same types, as a number
-- to keep.
overlapCode :: Overlap -> Int
overlapCode = \case
  Exactly -> 0
  Overlappable -> 1
  Overlapping -> 2
  Overlaps -> 3

overlapOf :: Int -> Overlap
overlapOf = \case
  1 -> Overlappable
  2 -> Overlapping
  3 -> Overlaps
  _ -> Exactly

-- | Compile a package. It is given its slot, the interfaces of the
-- packages whose modules it may import, the interfaces of the rest of
-- what those were compiled after, and its own modules. Each package below
-- it is to be there once, among one or the other.
compile :: Int -> [Interface] -> [Interface] -> [Unit Module] -> Either Failure Compiled
compile slot seen unseen units = do
  (hir, linked) <- first (Failure sources) (resolveAfter (Just seed) (Map.fromList [(unitPath u, unitExtensions u) | u <- units]) [] [(unitPath u, unitModule u, unitSyntax u) | u <- units] Nothing)
  let (found, problems) = typecheckAfter (ifaceSchemes below) [] hir
      wrong = kindcheck hir ++ problems
  unless (null wrong) (Left (Failure sources wrong))
  code <- first (\p -> Failure sources [p]) (lower hir found)
  pure (Compiled (interface hir linked found) code)
  where
    sources = [(unitPath u, unitSource u) | u <- units]
    below = mconcat (seen ++ unseen)
    start = slot * chunk
    own k = k >= start && k < start + chunk
    modules = Set.fromList (map unitModule units)
    seed =
      Seed
        { seedFloor = start,
          -- Only what the package depends on itself can be imported.
          seedLinkage = (ifaceLinkage below) {linkExports = mconcat (map (linkExports . ifaceLinkage) seen)},
          seedNames = ifaceNames below,
          seedOrigins = ifaceOrigins below,
          seedSignatures = ifaceSignatures below,
          seedSynonyms = ifaceSynonyms below,
          seedDataTypes = ifaceDataTypes below,
          seedClasses = IntMap.fromList [(nameKey c, Class c vs supers dependencies methods [Binding (NodeId (-1)) (Function m []) | m <- defaulted]) | ClassShape c vs supers dependencies methods defaulted <- ifaceClasses below],
          seedInstances = [Instance n c ts vs context [] (overlapOf overlap) | InstanceShape n c ts vs context overlap <- ifaceInstances below],
          seedNewtypes = ifaceNewtypes below,
          seedModules = ifaceModules below,
          seedImports = ifaceImports below
        }
    mineOf = IntMap.filterWithKey (\k _ -> own k)
    interface hir linked found =
      Interface
        { ifaceLinkage =
            Linkage
              { linkExports = Map.restrictKeys (linkExports linked) modules,
                linkBase = if "Powder.Base" `Set.member` modules then linkBase linked else Nothing,
                linkBelongings = mineOf (linkBelongings linked),
                -- What the language has is made by whichever package is first.
                linkWired = maybe (linkWired linked) (const Nothing) (linkWired (ifaceLinkage below)),
                linkFields = mineOf (linkFields linked),
                linkFixities = mineOf (linkFixities linked),
                linkStrict = mineOf (linkStrict linked)
              },
          ifaceNames = mineOf (HIR.programNames hir),
          ifaceOrigins = IntMap.fromList [(nodeKey (instanceId i), o) | i <- instances, Just o <- [HIR.origin hir (instanceId i)]],
          ifaceSignatures = mineOf (HIR.programSignatures hir),
          ifaceSynonyms = mineOf (HIR.programSynonyms hir),
          ifaceDataTypes = [d | d <- HIR.programDataTypes hir, own (nameKey (dataName d))],
          ifaceClasses =
            [ ClassShape (className c) (classVariables c) (classSupers c) (classDependencies c) (classMethods c) [m | Binding _ (Function m _) <- classDefaults c]
              | c <- IntMap.elems (mineOf (HIR.programClasses hir))
            ],
          ifaceInstances = [InstanceShape (instanceId i) (instanceClass i) (instanceHead i) (instanceVariables i) (instanceContext i) (overlapCode (instanceOverlap i)) | i <- instances],
          ifaceNewtypes = [c | c <- HIR.programNewtypes hir, own (nameKey c)],
          ifaceModules = [(file, m) | (file, m) <- HIR.programModules hir, m `Set.member` modules],
          ifaceImports = [(m, is) | (m, is) <- HIR.programImports hir, m `Set.member` modules],
          ifaceSchemes = mineOf (checkedSchemes found)
        }
      where
        instances = [i | i <- HIR.programInstances hir, own (nodeKey (instanceId i))]

-- | The name a program starts at: the @main@ of the module @Main@.
entry :: Name
entry = "main"

-- | The code of packages, put together into a program that starts at
-- 'entry'. The packages are every one the program is made of, each once.
link :: [Core.Program] -> Core.Program
link programs =
  Core.Program
    (Map.elems (Map.fromList [(conName c, c) | p <- programs, c <- Core.progCons p]))
    ((entry, App (Var "Powder.Base.top") (Var "Main.main")) : concatMap Core.progDefs programs)
    (concatMap Core.progExports programs)

-- What is kept in a file --------------------------------------------------

deriving instance Generic NameId

deriving instance Generic NodeId

deriving instance Generic NameInfo

deriving instance Generic NameSort

deriving instance Generic ConInfo

deriving instance Generic Origin

deriving instance Generic Pos

deriving instance Generic Signature

deriving instance Generic Constraint

deriving instance Generic HIR.Type.Type

deriving instance Generic HIR.Type.Form

deriving instance Generic DataType

deriving instance Generic Synonym

deriving instance Generic Builtins

deriving instance Generic Assoc

deriving instance Generic Scope

deriving instance Generic Linkage

deriving instance Generic Types.Type

deriving instance Generic Pred

deriving instance Generic Scheme

deriving instance Generic Core

deriving instance Generic Bind

deriving instance Generic Alt

deriving instance Generic Prim

deriving instance Generic Core.Program

deriving instance Generic Core.Export

instance Binary NameId

instance Binary NodeId

instance Binary NameInfo

instance Binary NameSort

instance Binary ConInfo

instance Binary Origin

instance Binary Pos

instance Binary Signature

instance Binary Constraint

instance Binary HIR.Type.Type

instance Binary HIR.Type.Form

instance Binary DataType

instance Binary Synonym

instance Binary Builtins

instance Binary Assoc

instance Binary Scope

instance Binary Linkage

instance Binary Types.Type

instance Binary Pred

instance Binary Scheme

instance Binary Core

instance Binary Bind

instance Binary Alt

instance Binary Prim

instance Binary Core.Program

instance Binary Core.Export
