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
import Data.Maybe (listToMaybe)
import Data.STRef
import Powder.Diagnostics (Problem (..))
import Powder.HIR
import Powder.HIR.Type (Type (..))
import qualified Powder.HIR.Type as Type

data Kind s
  = Star
  | Arrow (Kind s) (Kind s)
  | -- | A data type as a kind, applied to kinds: the kind of the types
    -- its constructors are.
    Promoted NameId [Kind s]
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
      OfClass c -> concatMap inConstraint (classSupers c) ++ concat [inSignature s | m <- classMethods c, Just s <- [signature program m]]
    inSignature (Signature _ context t) = concatMap inConstraint context ++ mentioned t
    inConstraint (Constraint c ts) = c : concatMap mentioned ts

    run = do
      -- What was said of kinds is so before any is found.
      forM_ (IntMap.toList (programKinds program)) $ \(n, said) -> unless (general said) $ do
        k <- kindOf (NameId n)
        written said >>= void . lift . unify k
      forM_ groups $ \group -> do
        mapM_ declaration group
        settle
      forM_ (IntMap.toList (programSignatures program)) $ \(n, s) ->
        unless (isMethod (NameId n)) (signed s)
      mapM_ instance' (programInstances program)
      mapM_ signed (concatMap annotations (allBindings program))
      forM_ (programKinded program) (\(t, said) -> written said >>= check t)
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
  Type.Forall _ context inner -> concat [c : concatMap mentioned ts | (c, ts) <- context] ++ mentioned inner
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
      Applied inner _ -> expr inner
      _ -> []

-- Plumbing ------------------------------------------------------------------

unknown :: K s (Kind s)
unknown = Unknown <$> lift (newSTRef Nothing)

-- | A kind as a program writes it. A variable in one stands for any
-- kind, and is a kind to be found each time it is read.
written :: KindSyntax -> K s (Kind s)
written = \case
  KindStar -> pure Star
  KindArrow a r -> Arrow <$> written a <*> written r
  KindNamed t -> pure (Promoted t [])
  KindList k -> do
    list <- asks (builtinList . programBuiltins . envProgram)
    Promoted list . pure <$> written k
  KindVariable _ -> unknown

-- | Whether a kind a program writes has a variable in it: then what has
-- it has a kind at each use, and not one for all of them.
general :: KindSyntax -> Bool
general = \case
  KindVariable _ -> True
  KindArrow a r -> general a || general r
  KindList k -> general k
  _ -> False

-- | The kind of a constructor used as a type: from the kinds its fields
-- are, as types, to its data type as a kind. A parameter of the data
-- type is any kind, the same wherever it is.
promoted :: NameId -> K s (Maybe (Kind s))
promoted c = do
  program <- asks envProgram
  case [(d, fields) | d <- programDataTypes program, (c', fields) <- dataConstructors d, c' == c] of
    (d, fields) : _ -> do
      parameters <- forM (dataParameters d) (\v -> (,) v <$> unknown)
      let asKind t = case spine t [] of
            (Type.Con k, arguments) -> Promoted k <$> mapM asKind arguments
            (Type.Var v, []) -> maybe unknown pure (lookup v parameters)
            (Type.Fun a b, []) -> Arrow <$> asKind a <*> asKind b
            _ -> unknown
          spine t arguments = case typeForm t of
            Type.App f a -> spine f (a : arguments)
            form -> (form, arguments)
      fields' <- mapM asKind fields
      pure (Just (foldr Arrow (Promoted (dataName d) (map snd parameters)) fields'))
    [] -> pure Nothing

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
    (Promoted t xs, Promoted u ys) | t == u && length xs == length ys -> and <$> zipWithM unify xs ys
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
        Promoted _ xs -> or <$> mapM (occurs v) xs
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
        Promoted _ xs -> mapM_ ground xs
        Star -> pure ()

render :: Kind s -> ST s String
render = go False
  where
    go nested k =
      shallow k >>= \case
        Star -> pure "*"
        Unknown _ -> pure "_"
        Promoted t xs -> do
          xs' <- mapM (go True) xs
          pure (unwords (("a data type's, " ++ show (nameKey t)) : xs'))
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
  -- One said to be of a kind with a variable in it is of any such, here.
  Type.Var a -> saidGenerally a >>= maybe (kindOf a) written
  -- A constructor is a type of its data type's kind; and a type declared
  -- with a kind that has a variable in it has a kind for each use.
  Type.Con c -> promoted c >>= maybe (ofDeclared c) pure
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
  -- Its variables are of whatever kinds it uses them at, and it is a
  -- type.
  Type.Forall _ context inner -> Star <$ (mapM_ (\(c, ts) -> constraint (Constraint c ts)) context >> check inner Star)
  Type.Error -> unknown

-- | What a name was said to be of, if that is a kind with a variable in
-- it.
saidGenerally :: NameId -> K s (Maybe KindSyntax)
saidGenerally n = asks (\env -> case IntMap.lookup (nameKey n) (programKinds (envProgram env)) of
  Just said | general said -> Just said
  _ -> Nothing)

-- | The kind of a declared type. One whose parameters were said to be of
-- kinds with variables in them takes, at each use, parameters of any
-- such kinds.
ofDeclared :: NameId -> K s (Kind s)
ofDeclared c = do
  program <- asks envProgram
  said <- saidGenerally c
  let parameters = listToMaybe [dataParameters d | d <- programDataTypes program, dataName d == c]
      saidOf v = IntMap.lookup (nameKey v) (programKinds program)
  case (said, parameters) of
    (Just whole, _) -> written whole
    (Nothing, Just vs) | any (maybe False general . saidOf) vs -> do
      ks <- forM vs (maybe unknown written . saidOf)
      pure (foldr Arrow Star ks)
    _ -> kindOf c

-- | Check a type against the kind it must have.
check :: Type -> Kind s -> K s ()
check t wanted = do
  found <- infer t
  ok <- lift (unify found wanted)
  unless ok $ do
    has <- lift (render found)
    needs <- lift (render wanted)
    complainAt (typeId t) ("this type has kind " ++ has ++ ", but one of kind " ++ needs ++ " is needed here")

-- | A class asked of types: each has the kind of the class's variable at
-- its place.
constraint :: Constraint -> K s ()
constraint (Constraint c ts) = do
  program <- asks envProgram
  forM_ (classOf program c) $ \cls -> zipWithM_ (\t v -> (saidGenerally v >>= maybe (kindOf v) written) >>= check t) ts (classVariables cls)

-- Declarations --------------------------------------------------------------

declaration :: Declared -> K s ()
declaration = \case
  -- A data type takes its parameters to a type, and its fields are types.
  OfData d -> do
    parameters <- mapM kindOf (dataParameters d)
    kindOf (dataName d) >>= \k -> void (lift (unify k (foldr Arrow Star parameters)))
    forM_ (dataConstructors d) (mapM_ (`check` Star) . snd)
    forM_ (dataExistentials d) (mapM_ constraint . snd . snd)
  -- A synonym takes its parameters to whatever it stands for.
  OfSynonym n s -> do
    parameters <- mapM kindOf (synonymParameters s)
    body <- infer (synonymType s)
    kindOf n >>= \k -> void (lift (unify k (foldr Arrow body parameters)))
  -- A class takes its variables, whose kinds its superclasses and the
  -- types of its methods say.
  OfClass c -> do
    program <- asks envProgram
    variables <- mapM kindOf (classVariables c)
    kindOf (className c) >>= \k -> void (lift (unify k (foldr Arrow Star variables)))
    mapM_ constraint (classSupers c)
    forM_ (classMethods c) (mapM_ signed . signature program)

-- | A signature: what its context asks is of the right kinds, and what
-- it says is a type.
signed :: Signature -> K s ()
signed (Signature _ context t) = mapM_ constraint context >> check t Star

-- | An instance: each of its types is of the kind of the class's
-- variable at its place.
instance' :: Instance -> K s ()
instance' inst = do
  program <- asks envProgram
  forM_ (classOf program (instanceClass inst)) $ \cls ->
    forM_ (zip (instanceHead inst) (classVariables cls)) $ \(t, v) -> do
      wanted <- saidGenerally v >>= maybe (kindOf v) written
      found <- infer t
      fits <- lift (unify found wanted)
      unless fits $ do
        needs <- lift (render wanted)
        complainAt (instanceId inst) ("the type of this instance is not of the kind the class's instances are, which is " ++ needs)
  mapM_ constraint (instanceContext inst)
