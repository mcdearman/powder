-- | Unification, by union-find with path compression, in 'ST'.
--
-- While types are being worked out, an unknown is a node in a forest. Two
-- unknowns found to be the same are joined, the root of one tree made a
-- child of the other's; the root of a tree holds what its unknowns are
-- known to be, once they are. Finding an unknown's root makes every node
-- on the way a child of the root, so that the next search is short, and
-- trees are joined shorter under taller, so that they stay shallow.
--
-- These types only live inside one run of the checker. What it hands back
-- is 'Powder.Types.Type', which 'zonk' makes.
module Powder.Unify
  ( Type (..),
    Var,
    key,
    newVar,
    shallow,
    spine,
    arrow,
    unify,
    solve,
    zonk,
    unsolved,
    rigids,
    substitute,
  )
where

import Control.Monad (when)
import Control.Monad.ST
import qualified Data.IntMap.Strict as IntMap
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.STRef
import Powder.HIR.Id (NameId (..))
import qualified Powder.Types as Types

-- | The function arrow, as the type constructor it is when it is written
-- by itself: @(->) a b@ is @a -> b@. A function type is kept as 'Fun',
-- and is the arrow applied to two types wherever that is how it is met.
arrow :: NameId
arrow = NameId (-1)

-- | A type that may have unknowns in it.
data Type s
  = Con NameId
  | App (Type s) (Type s)
  | Fun (Type s) (Type s)
  | -- | A type variable that stands for itself.
    Rigid NameId
  | -- | An unknown.
    Meta (Var s)

-- | An unknown: a node of the forest.
data Var s = Var {key :: Int, node :: STRef s (Node s)}

instance Eq (Var s) where
  a == b = key a == key b

data Node s
  = -- | The same as another unknown, nearer the root.
    Child (Var s)
  | -- | A root: its rank, which bounds the height of its tree, and what
    -- its unknowns are, if that is known. That is never just an unknown:
    -- two unknowns that are the same are in one tree.
    Root Int ~(Maybe (Type s))

-- | A new unknown, given a number no other has.
newVar :: Int -> ST s (Var s)
newVar k = Var k <$> newSTRef (Root 0 Nothing)

-- | The root of an unknown's tree. Every node passed on the way is made a
-- child of the root.
find :: Var s -> ST s (Var s)
find v =
  readSTRef (node v) >>= \case
    Root _ _ -> pure v
    Child parent -> do
      root <- find parent
      root <$ writeSTRef (node v) (Child root)

-- | What a root's unknowns are known to be.
solution :: Var s -> ST s (Maybe (Type s))
solution root =
  readSTRef (node root) >>= \case
    Root _ known -> pure known
    Child _ -> pure Nothing

-- | A type with its outermost unknown seen through: what it is known to
-- be, or failing that the root of its tree.
shallow :: Type s -> ST s (Type s)
shallow = \case
  Meta v -> do
    root <- find v
    fromMaybe (Meta root) <$> solution root
  t -> pure t

-- | A type as what is at its head and what that is applied to.
spine :: Type s -> ST s (Type s, [Type s])
spine t = go t []
  where
    go u arguments =
      shallow u >>= \case
        App f a -> go f (a : arguments)
        Fun a r | null arguments -> pure (Con arrow, [a, r])
        hd -> pure (hd, arguments)

-- | Say what an unknown is. It must be a root with nothing known of it.
solve :: Var s -> Type s -> ST s ()
solve root t = modifySTRef' (node root) $ \case
  Root r _ -> Root r (Just t)
  child -> child

-- | Make two types the same, by joining and solving unknowns. Says whether
-- they could be: they cannot if they differ where neither is unknown, or
-- if an unknown would have to contain itself.
unify :: Type s -> Type s -> ST s Bool
unify a b = do
  a' <- shallow a
  b' <- shallow b
  case (a', b') of
    (Meta v, Meta w)
      | v == w -> pure True
      | otherwise -> True <$ union v w
    (Meta v, t) -> bind v t
    (t, Meta v) -> bind v t
    (Con c, Con d) -> pure (c == d)
    (Rigid x, Rigid y) -> pure (x == y)
    (App f x, App g y) -> (&&) <$> unify f g <*> unify x y
    (Fun x r, Fun y s) -> (&&) <$> unify x y <*> unify r s
    (App f x, Fun y s) -> (&&) <$> unify f (App (Con arrow) y) <*> unify x s
    (Fun y s, App f x) -> (&&) <$> unify (App (Con arrow) y) f <*> unify s x
    _ -> pure False
  where
    bind v t = do
      inside <- occurs v t
      if inside then pure False else True <$ solve v t

    -- Join two trees, neither with anything known: the shorter under the
    -- taller.
    union v w = do
      rankV <- rank v
      rankW <- rank w
      if rankV < rankW
        then writeSTRef (node v) (Child w)
        else do
          writeSTRef (node w) (Child v)
          when (rankV == rankW) (writeSTRef (node v) (Root (rankV + 1) Nothing))

-- | A root's rank.
rank :: Var s -> ST s Int
rank v =
  readSTRef (node v) >>= \case
    Root r _ -> pure r
    Child _ -> pure 0

-- | Whether an unknown is anywhere in a type.
occurs :: Var s -> Type s -> ST s Bool
occurs v t =
  shallow t >>= \case
    Meta w -> pure (v == w)
    App f x -> (||) <$> occurs v f <*> occurs v x
    Fun x r -> (||) <$> occurs v x <*> occurs v r
    Con _ -> pure False
    Rigid _ -> pure False

-- | The type as it stands, with everything known put in. What is still
-- unknown is left as the number of its root.
zonk :: Type s -> ST s Types.Type
zonk t =
  shallow t >>= \case
    Meta v -> pure (Types.Meta (key v))
    Con c -> pure (Types.Con c)
    Rigid a -> pure (Types.Rigid a)
    App f x -> Types.App <$> zonk f <*> zonk x
    Fun x r -> Types.Fun <$> zonk x <*> zonk r

-- | The unknowns still in a type, as their roots, in order, each once.
unsolved :: Type s -> ST s [Var s]
unsolved t = nub <$> go t
  where
    go u =
      shallow u >>= \case
        Meta v -> pure [v]
        App f x -> (++) <$> go f <*> go x
        Fun x r -> (++) <$> go x <*> go r
        Con _ -> pure []
        Rigid _ -> pure []

-- | The type variables in a type, in order.
rigids :: Type s -> ST s [NameId]
rigids t =
  shallow t >>= \case
    Rigid a -> pure [a]
    App f x -> (++) <$> rigids f <*> rigids x
    Fun x r -> (++) <$> rigids x <*> rigids r
    Con _ -> pure []
    Meta _ -> pure []

-- | Put types in place of type variables.
substitute :: IntMap.IntMap (Type s) -> Type s -> ST s (Type s)
substitute s t =
  shallow t >>= \case
    Rigid a -> pure (IntMap.findWithDefault (Rigid a) (nameKey a) s)
    App f x -> App <$> substitute s f <*> substitute s x
    Fun x r -> Fun <$> substitute s x <*> substitute s r
    other -> pure other
