-- | Kinds: that every type is put together the way its parts allow.
--
-- A type constructor has a kind, which says what it may be applied to,
-- and so has a class, whose kind is that of the types that may be its
-- instances. Kinds are found as the Report says: the declarations of
-- types and classes are taken in groups that depend on each other, the
-- kinds in a group are worked out together by unification, and what is
-- still undetermined when a group is done is @*@. Then every signature,
-- instance and annotation is checked against the kinds found.
--
-- Like the type checker, this runs in 'ST', with an unknown kind a
-- reference that is filled in when it is known.
module Powder.Kinds (kindcheck) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.ST
import Data.Graph (flattenSCC, stronglyConnComp)
import qualified Data.IntMap.Strict as IntMap
import Data.STRef
import Powder.Diagnostics (Problem (..))
import Powder.HIR
import Powder.HIR.Type (Type (..))
import qualified Powder.HIR.Type as Type

data Kind s
  = Star
  | Arrow (Kind s) (Kind s)
  | -- | Not yet known.
    Unknown (STRef s (Maybe (Kind s)))

data Env s = Env
  { envProgram :: Program,
    -- | The kind of each type constructor, class and type variable met
    -- so far, by 'NameId'.
    refKinds :: STRef s (Table (Kind s)),
    -- | Most recent first.
    refProblems :: STRef s [Problem]
  }

type K s = ReaderT (Env s) (ST s)

-- | A declaration that gives something a kind.
data Declared = OfData DataType | OfSynonym NameId Synonym | OfClass Class

-- | What is wrong with the kinds of a program.
kindcheck :: Program -> [Problem]
kindcheck program = runST $ do
  -- The function arrow is no declaration's: it takes two types to a type.
  kinds <- newSTRef (IntMap.singleton (-1) (Arrow Star (Arrow Star Star)))
  problems <- newSTRef []
  runReaderT run (Env program kinds problems)
  reverse <$> readSTRef problems
  where
    declared =
      map OfData (programDataTypes program)
        ++ [OfSynonym (NameId n) s | (n, s) <- IntMap.toList (programSynonyms program)]
        ++ map OfClass (IntMap.elems (programClasses program))
    groups = map flattenSCC (stronglyConnComp [(d, nameKey (nameOf d), map nameKey (uses d)) | d <- declared])

    nameOf = \case
      OfData d -> dataName d
      OfSynonym n _ -> n
      OfClass c -> className c
    uses = \case
      OfData d -> concatMap mentioned (concatMap snd (dataConstructors d))
      OfSynonym _ s -> mentioned (synonymType s)
      OfClass c -> classSupers c ++ concat [inSignature s | m <- classMethods c, Just s <- [signature program m]]
    inSignature (Signature _ context t) = map constraintClass context ++ concatMap (mentioned . constraintType) context ++ mentioned t

    run = do
      forM_ groups $ \group -> do
        mapM_ declaration group
        settle
      forM_ (IntMap.toList (programSignatures program)) $ \(n, s) ->
        unless (isMethod (NameId n)) (signed s)
      mapM_ instance' (programInstances program)
      mapM_ signed (concatMap annotations (allBindings program))
      settle

    isMethod n = case nameSort <$> nameInfo program n of
      Just (Method _) -> True
      _ -> False

-- | The type constructors a type mentions.
mentioned :: Type -> [NameId]
mentioned t = case typeForm t of
  Type.Con c -> [c]
  Type.App f a -> mentioned f ++ mentioned a
  Type.Fun a b -> mentioned a ++ mentioned b
  Type.Var _ -> []
  Type.Error -> []

-- | The signatures expressions are said to have, anywhere in a binding.
annotations :: Binding -> [Signature]
annotations = binding
  where
    binding b = case bindingKind b of
      Function _ clauses -> concatMap (rhs . clauseRhs) clauses
      Destructure _ r -> rhs r
    rhs r = concatMap guarded (rhsGuarded r) ++ concatMap binding (rhsWhere r)
    guarded g = concatMap qualifier (guardedQualifiers g) ++ expr (guardedBody g)
    qualifier = \case
      Condition c -> expr c
      Match _ e -> expr e
      Bindings bs -> concatMap binding bs
    expr e = case exprKind e of
      App f a -> expr f ++ expr a
      Lam _ body -> expr body
      Let bs body -> concatMap binding bs ++ expr body
      If c t f -> expr c ++ expr t ++ expr f
      Case scrutinee alts -> expr scrutinee ++ concatMap (rhs . altRhs) alts
      Annotated inner s -> s : expr inner
      _ -> []

-- Plumbing ------------------------------------------------------------------

unknown :: K s (Kind s)
unknown = Unknown <$> lift (newSTRef Nothing)

-- | The kind of a name: the one it was found to have, or one to find.
kindOf :: NameId -> K s (Kind s)
kindOf n = do
  ref <- asks refKinds
  found <- lift (IntMap.lookup (nameKey n) <$> readSTRef ref)
  case found of
    Just k -> pure k
    Nothing -> do
      k <- unknown
      k <$ lift (modifySTRef' ref (IntMap.insert (nameKey n) k))

shallow :: Kind s -> ST s (Kind s)
shallow = \case
  Unknown ref ->
    readSTRef ref >>= \case
      Just k -> do
        k' <- shallow k
        k' <$ writeSTRef ref (Just k')
      Nothing -> pure (Unknown ref)
  k -> pure k

-- | Make two kinds the same, if they can be.
unify :: Kind s -> Kind s -> ST s Bool
unify a b = do
  a' <- shallow a
  b' <- shallow b
  case (a', b') of
    (Star, Star) -> pure True
    (Arrow x r, Arrow y s) -> (&&) <$> unify x y <*> unify r s
    (Unknown v, Unknown w) | v == w -> pure True
    (Unknown v, k) -> bind v k
    (k, Unknown v) -> bind v k
    _ -> pure False
  where
    bind v k = do
      inside <- occurs v k
      if inside then pure False else True <$ writeSTRef v (Just k)
    occurs v k =
      shallow k >>= \case
        Unknown w -> pure (v == w)
        Arrow x r -> (||) <$> occurs v x <*> occurs v r
        Star -> pure False

-- | What is still undetermined is @*@.
settle :: K s ()
settle = do
  kinds <- asks refKinds >>= lift . readSTRef
  lift (mapM_ ground (IntMap.elems kinds))
  where
    ground k =
      shallow k >>= \case
        Unknown ref -> writeSTRef ref (Just Star)
        Arrow a r -> ground a >> ground r
        Star -> pure ()

render :: Kind s -> ST s String
render = go False
  where
    go nested k =
      shallow k >>= \case
        Star -> pure "*"
        Unknown _ -> pure "_"
        Arrow a r -> do
          a' <- go True a
          r' <- go False r
          pure (if nested then "(" ++ a' ++ " -> " ++ r' ++ ")" else a' ++ " -> " ++ r')

complainAt :: NodeId -> String -> K s ()
complainAt n message = do
  program <- asks envProgram
  let from = origin program n
  ref <- asks refProblems
  lift (modifySTRef' ref (Problem "kind error" message (maybe "" originFile from) (originSpan <$> from) False :))

-- Types ---------------------------------------------------------------------

-- | The kind of a type.
infer :: Type -> K s (Kind s)
infer t = case typeForm t of
  Type.Var a -> kindOf a
  Type.Con c -> kindOf c
  Type.Fun a b -> Star <$ (check a Star >> check b Star)
  Type.App f a -> do
    function <- infer f
    argument <- infer a
    result <- unknown
    ok <- lift (unify function (Arrow argument result))
    unless ok $ do
      lift (shallow function) >>= \case
        Arrow wanted _ -> do
          has <- lift (render argument)
          needs <- lift (render wanted)
          complainAt (typeId a) ("this type has kind " ++ has ++ ", but it is given to a type that takes one of kind " ++ needs)
        other -> do
          has <- lift (render other)
          complainAt (typeId f) ("this type has kind " ++ has ++ ", so it cannot be applied to a type")
    pure result
  Type.Error -> unknown

-- | Check a type against the kind it must have.
check :: Type -> Kind s -> K s ()
check t wanted = do
  found <- infer t
  ok <- lift (unify found wanted)
  unless ok $ do
    has <- lift (render found)
    needs <- lift (render wanted)
    complainAt (typeId t) ("this type has kind " ++ has ++ ", but one of kind " ++ needs ++ " is needed here")

-- | A class asked of a type: the type has the kind of the class's
-- instances.
constraint :: Constraint -> K s ()
constraint (Constraint c t) = kindOf c >>= check t

-- Declarations --------------------------------------------------------------

declaration :: Declared -> K s ()
declaration = \case
  -- A data type takes its parameters to a type, and its fields are types.
  OfData d -> do
    parameters <- mapM kindOf (dataParameters d)
    kindOf (dataName d) >>= \k -> void (lift (unify k (foldr Arrow Star parameters)))
    forM_ (dataConstructors d) (mapM_ (`check` Star) . snd)
  -- A synonym takes its parameters to whatever it stands for.
  OfSynonym n s -> do
    parameters <- mapM kindOf (synonymParameters s)
    body <- infer (synonymType s)
    kindOf n >>= \k -> void (lift (unify k (foldr Arrow body parameters)))
  -- A class's kind is its variable's, which its superclasses and the
  -- types of its methods say.
  OfClass c -> do
    program <- asks envProgram
    variable <- kindOf (classVariable c)
    kindOf (className c) >>= \k -> void (lift (unify k variable))
    forM_ (classSupers c) (kindOf >=> (\k -> void (lift (unify k variable))))
    forM_ (classMethods c) (mapM_ signed . signature program)

-- | A signature: what its context asks is of the right kinds, and what
-- it says is a type.
signed :: Signature -> K s ()
signed (Signature _ context t) = mapM_ constraint context >> check t Star

-- | An instance: its type is of the kind of the class's instances.
instance' :: Instance -> K s ()
instance' inst = do
  variables <- mapM kindOf (instanceVariables inst)
  constructor <- kindOf (instanceType inst)
  result <- unknown
  wanted <- kindOf (instanceClass inst)
  applies <- lift (unify constructor (foldr Arrow result variables))
  fits <- if applies then lift (unify result wanted) else pure False
  unless fits $ do
    needs <- lift (render wanted)
    complainAt (instanceId inst) ("the type of this instance is not of the kind the class's instances are, which is " ++ needs)
  mapM_ constraint (instanceContext inst)
