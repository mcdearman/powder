-- | The type checker.
--
-- It is bidirectional, in the manner of GHC's and of Peyton Jones,
-- Vytiniotis, Weirich and Shields's "Practical type inference for
-- arbitrary-rank types". An expression is either asked for its type
-- ('infer') or told it ('check'). Telling is how a signature's type
-- reaches the lambdas and alternatives inside a definition, where an
-- error is then reported against what was declared rather than against
-- whatever unification happened to work out first. Where nothing is told,
-- types are found as Hindley and Milner would: unknowns, solved by
-- unification, and generalised at each binding with no signature.
--
-- Its result is a table: the type of each node of the HIR, under the
-- node's identity. Nothing is written back into the tree.
--
-- It runs in 'ST'. Unknowns are solved in place, in the union-find forest
-- of "Powder.Unify", and what it notes on the way it keeps in references.
--
-- A mistake does not stop it. The mistake is reported, and checking goes
-- on with the types as they stand.
--
-- Classes are checked by passing dictionaries, though nothing is passed
-- here: where an overloaded name is used, the checker notes that the type
-- it is used at must be an instance of a class, and when it knows the
-- type it notes beside the use why it is, which is evidence. A definition
-- with a context is noted to take a parameter for each class the context
-- asks. The lowering to Core turns the notes into arguments and lambdas.
--
-- Not yet here: kinds; and types of rank above one, which the two modes
-- are there to carry.
module Powder.Typecheck
  ( Checked (..),
    typecheck,
    typecheckAfter,
    typeOf,
    schemeOf,
    evidenceAt,
    parametersOf,
  )
where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.Reader
import Control.Monad.ST
import Data.Graph (flattenSCC, stronglyConnComp)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (nub, partition, (\\))
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, maybeToList)
import Data.STRef
import Powder.Core (Prim (..))
import Powder.Diagnostics (Problem (..))
import Powder.HIR hiding (ExprKind (..))
import qualified Powder.HIR as HIR
import qualified Powder.HIR.Pat as Pat
import qualified Powder.HIR.Type as Syntax
import Powder.Types (Evidence, Scheme (..), prettyType)
import qualified Powder.Types as Types
import Powder.Unify (Type (..))
import qualified Powder.Unify as Unify

-- | What checking found.
data Checked = Checked
  { -- | The type of each node, by 'NodeId'.
    checkedTypes :: Table Types.Type,
    -- | The type of each name a program binds, by 'NameId'.
    checkedSchemes :: Table Scheme,
    -- | The dictionaries a node is to be given, by 'NodeId'. For a use of
    -- an overloaded name, those its context asks, in order. For a number,
    -- that of its type as a number, after that of its equality if it is
    -- a pattern. For an instance, those of its class's superclasses.
    checkedEvidence :: Table [Evidence],
    -- | The dictionaries a definition or an instance takes, as the
    -- numbers evidence knows them by, by 'NodeId'.
    checkedParameters :: Table [Int]
  }

typeOf :: Checked -> NodeId -> Maybe Types.Type
typeOf checked (NodeId n) = IntMap.lookup n (checkedTypes checked)

schemeOf :: Checked -> NameId -> Maybe Scheme
schemeOf checked (NameId n) = IntMap.lookup n (checkedSchemes checked)

evidenceAt :: Checked -> NodeId -> [Evidence]
evidenceAt checked (NodeId n) = IntMap.findWithDefault [] n (checkedEvidence checked)

parametersOf :: Checked -> NodeId -> [Int]
parametersOf checked (NodeId n) = IntMap.findWithDefault [] n (checkedParameters checked)

-- | A class asked of a type.
data Pred s = Pred NameId (Type s)

-- | A type for all types its variables could be that are instances of
-- what its context asks, while it may still have unknowns in it.
data Poly s = Poly [NameId] [Pred s] (Type s)

-- | Evidence while it is being found: some of it is not known yet.
data Ev s
  = EvParameter Int
  | EvInstance NodeId [Ev s]
  | EvSuper NameId Int (Ev s)
  | -- | To be filled in when the type it is for is known.
    EvHole (STRef s (Maybe (Ev s)))

-- | Something a type must be an instance of, and where the evidence that
-- it is goes.
data Wanted s = Wanted
  { wantedClass :: NameId,
    wantedType :: Type s,
    wantedHole :: STRef s (Maybe (Ev s)),
    -- | What asked for it.
    wantedNode :: Maybe NodeId
  }

-- | Something a type is known to be an instance of: a signature said so.
data Given s = Given NameId (Type s) (Ev s)

data Env s = Env
  { envProgram :: Program,
    -- | The types of the names in scope.
    envSchemes :: Table (Poly s),
    -- | The types in scope that may hold unknowns: those of names not yet
    -- generalised. An unknown in one of them is not free to generalise.
    envMonos :: [Type s],
    -- | The node being checked, for placing what is found wrong.
    envNode :: Maybe NodeId,
    envGivens :: [Given s],
    -- | The names whose types are being inferred: those of the groups
    -- being checked that have no signature.
    envInferring :: IntSet.IntSet,
    -- | The instances, by class and type constructor.
    envInstances :: Map.Map (Int, Int) Instance,
    -- | The names the monomorphism restriction is not to hold for.
    envGeneral :: [NameId],
    -- | The module a node is in, if that is known, and the modules each
    -- module's imports lead to.
    envModuleOf :: Maybe NodeId -> Maybe String,
    envReach :: Map.Map String [String],
    -- What is noted on the way.
    refNext :: STRef s Int,
    refTypes :: STRef s (Table (Type s)),
    refSchemes :: STRef s (Table (Poly s)),
    -- | Most recent first.
    refProblems :: STRef s [Problem],
    -- | What is wanted and not yet seen to. Most recent first.
    refWanted :: STRef s [Wanted s],
    refEvidence :: STRef s (Table [Ev s]),
    refParameters :: STRef s (Table [Int]),
    -- | Where names being inferred are used, which is where they will
    -- need what their groups turn out to take.
    refUses :: STRef s [(NameId, NodeId)],
    -- | The numbers.
    refLiterals :: STRef s [NodeId]
  }

type Tc s = ReaderT (Env s) (ST s)

-- | Check a program, giving the types found and what is wrong with it.
-- The names given are generalised even where the monomorphism restriction
-- would have them not be: a prompt asks an expression's type that way.
typecheck :: [NameId] -> Program -> (Checked, [Problem])
typecheck = typecheckAfter IntMap.empty

-- | Check a program that is a package compiled after others, given the
-- types of what the others define. What is theirs in the program is
-- taken as checked.
typecheckAfter :: Table Scheme -> [NameId] -> Program -> (Checked, [Problem])
typecheckAfter theirs general program = runST $ do
  -- Type variables made here must not be taken for the program's own.
  next <- newSTRef (1 + maximum (0 : IntMap.keys (programNames program) ++ IntMap.keys (programOrigins program)))
  types <- newSTRef IntMap.empty
  schemes <- newSTRef IntMap.empty
  problems <- newSTRef []
  wanted <- newSTRef []
  evidence <- newSTRef IntMap.empty
  parameters <- newSTRef IntMap.empty
  uses <- newSTRef []
  literals <- newSTRef []
  let instances = Map.fromList [((nameKey (instanceClass i), nameKey (instanceType i)), i) | i <- programInstances program]
  runReaderT run (Env program IntMap.empty [] Nothing [] IntSet.empty instances general moduleOf reach next types schemes problems wanted evidence parameters uses literals)
  foundTypes <- readSTRef types >>= traverse Unify.zonk
  foundSchemes <- readSTRef schemes >>= traverse zonkPoly
  foundEvidence <- readSTRef evidence >>= traverse (mapM zonkEvidence)
  foundParameters <- readSTRef parameters
  found <- readSTRef problems
  -- A number that is an integer is one already, and needs no making.
  let integers = [builtinInteger (programBuiltins program), builtinInt (programBuiltins program)]
      plain n = case IntMap.lookup (nodeKey n) foundTypes of
        Just (Types.Con c) -> c `elem` integers
        _ -> False
  numbers <- filter plain <$> readSTRef literals
  pure (Checked foundTypes foundSchemes (foldr (IntMap.delete . nodeKey) foundEvidence numbers) foundParameters, reverse found)
  where
    moduleOf n = n >>= origin program >>= \o -> lookup (originFile o) (programModules program)
    imports = Map.fromList (programImports program)
    -- Everything a module's imports lead to.
    reach = Map.fromList [(m, reached [] (Map.findWithDefault [] m imports)) | m <- Map.keys imports]
    reached seen = \case
      [] -> seen
      m : rest
        | m `elem` seen -> reached seen rest
        | otherwise -> reached (m : seen) (Map.findWithDefault [] m imports ++ rest)
    -- The modules in the groups that import each other, those a group
    -- imports before it. Each group is checked as a unit: its types are
    -- settled before the next is looked at, so that what a later module
    -- does with a name cannot change what the name's type is.
    units = map flattenSCC (stronglyConnComp [(m, m, is) | (m, is) <- programImports program])
    unitOf m = length (takeWhile (\u -> maybe True (`notElem` u) m) units)
    inUnit i m = min (length units - 1) (unitOf m) == i || null units

    run = do
      constructors <- concat <$> mapM constructorSchemes (programDataTypes program)
      let classes = IntMap.elems (programClasses program)
      methods <- fmap catMaybes . forM (concatMap classMethods classes) $ \m -> fmap (m,) <$> declared m
      forM_ methods (uncurry recordScheme)
      known <- traverse thaw theirs
      local (\env -> env {envSchemes = IntMap.union (IntMap.fromList (constructors ++ [(nameKey m, p) | (m, p) <- methods])) known}) $ do
        -- What a default declaration names must be a number.
        forM_ (concatMap snd (programDefaults program)) $ \t -> at (Syntax.typeId t) $ do
          (hd, _) <- fromSyntax t >>= lift . Unify.spine
          known <- asks envInstances
          case (hd, builtinNumeric (programBuiltins program)) of
            (Con c, Just (num, _)) | Map.notMember (nameKey num, nameKey c) known -> complain "a default declaration names types that are instances of Num, and this is not one"
            _ -> pure ()
        foldr unit (pure ()) [0 .. max 0 (length units - 1)]
      settle
      where
        unit i rest =
          bindings [b | b <- programBindings program, inUnit i (moduleOf (Just (bindingId b)))] $ do
            forM_ (IntMap.elems (programClasses program)) $ \cls ->
              when (mine program (nameKey (className cls)) && inUnit i (nameModule <$> nameInfo program (className cls))) (mapM_ binding (classDefaults cls))
            forM_ (programInstances program) (\inst -> when (mine program (nodeKey (instanceId inst)) && inUnit i (moduleOf (Just (instanceId inst)))) (instance' inst))
            settle
            rest

    zonkPoly (Poly vs context t) = Forall vs <$> mapM (\(Pred c a) -> Types.Pred c <$> Unify.zonk a) context <*> Unify.zonk t

    -- A type found before, as one to check against.
    thaw (Forall vs context t) = Poly vs <$> mapM (\(Types.Pred c a) -> Pred c <$> thawed a) context <*> thawed t
    thawed = \case
      Types.Con c -> pure (Con c)
      Types.App f a -> App <$> thawed f <*> thawed a
      Types.Fun a b -> Fun <$> thawed a <*> thawed b
      Types.Rigid v -> pure (Rigid v)
      Types.Meta _ -> unknown

zonkEvidence :: Ev s -> ST s Evidence
zonkEvidence = \case
  EvParameter d -> pure (Types.Parameter d)
  EvInstance i es -> Types.Instance i <$> mapM zonkEvidence es
  EvSuper c i e -> Types.Superclass c i <$> zonkEvidence e
  EvHole ref -> readSTRef ref >>= maybe (pure Types.Missing) zonkEvidence

-- Plumbing ------------------------------------------------------------------

at :: NodeId -> Tc s a -> Tc s a
at n = local (\env -> env {envNode = Just n})

-- | Change what a reference holds.
note :: (Env s -> STRef s a) -> (a -> a) -> Tc s ()
note ref f = asks ref >>= \r -> lift (modifySTRef' r f)

complain :: String -> Tc s ()
complain message = do
  env <- ask
  let from = envNode env >>= origin (envProgram env)
  note refProblems (Problem "type error" message (maybe "" originFile from) (originSpan <$> from) False :)

freshKey :: Tc s Int
freshKey = do
  ref <- asks refNext
  lift (readSTRef ref <* modifySTRef' ref (+ 1))

unknown :: Tc s (Type s)
unknown = freshKey >>= fmap Meta . lift . Unify.newVar

-- | Note a node's type.
record :: NodeId -> Type s -> Tc s ()
record (NodeId n) t = note refTypes (IntMap.insert n t)

-- | Note the type of a name the program binds.
recordScheme :: NameId -> Poly s -> Tc s ()
recordScheme (NameId x) scheme = note refSchemes (IntMap.insert x scheme)

-- | Note the dictionaries a node is to be given.
recordEvidence :: NodeId -> [Ev s] -> Tc s ()
recordEvidence (NodeId n) evidence = unless (null evidence) (note refEvidence (IntMap.insert n evidence))

-- | Note the dictionaries a node takes.
recordParameters :: NodeId -> [Int] -> Tc s ()
recordParameters (NodeId n) ds = unless (null ds) (note refParameters (IntMap.insert n ds))

showType :: Type s -> Tc s String
showType t = asks (prettyType . envProgram) <*> lift (Unify.zonk t)

builtin :: (Builtins -> NameId) -> Tc s (Type s)
builtin which = asks (Con . which . programBuiltins . envProgram)

listOf :: Type s -> Tc s (Type s)
listOf element = (`App` element) <$> builtin builtinList

-- | The type a name in scope has. One that is not there is one something
-- earlier got wrong and has reported; its type is anyone's guess.
schemeOfName :: NameId -> Tc s (Poly s)
schemeOfName x = asks (IntMap.lookup (nameKey x) . envSchemes) >>= maybe (Poly [] [] <$> unknown) pure

-- | Bring names into scope with types that are not generalised.
withMonos :: [(NameId, Type s)] -> Tc s a -> Tc s a
withMonos binds = local $ \env ->
  env
    { envSchemes = IntMap.union (IntMap.fromList [(nameKey x, Poly [] [] t) | (x, t) <- binds]) (envSchemes env),
      envMonos = map snd binds ++ envMonos env
    }

withSchemes :: [(NameId, Poly s)] -> Tc s a -> Tc s a
withSchemes binds = local (\env -> env {envSchemes = IntMap.union (IntMap.fromList [(nameKey x, p) | (x, p) <- binds]) (envSchemes env)})

-- Unification ---------------------------------------------------------------

-- | Make two types the same. If they cannot be, say so and leave them be.
unify :: Type s -> Type s -> Tc s ()
unify expected actual = do
  ok <- lift (Unify.unify expected actual)
  unless ok $ do
    e <- showType expected
    a <- showType actual
    complain ("expected " ++ e ++ ", but this has type " ++ a)

-- | See a type as a function's: the types of its argument and its result.
unifyFun :: Type s -> Tc s (Type s, Type s)
unifyFun t =
  lift (Unify.shallow t) >>= \case
    Fun a r -> pure (a, r)
    other -> do
      a <- unknown
      r <- unknown
      (a, r) <$ unify other (Fun a r)

-- | A scheme at fresh unknowns: one use of something polymorphic. What
-- its context asks of them is wanted, and the evidence is for the node
-- being checked.
instantiate :: Poly s -> Tc s (Type s)
instantiate (Poly [] [] t) = pure t
instantiate (Poly vs context t) = do
  fresh <- IntMap.fromList <$> forM vs (\v -> (,) (nameKey v) <$> unknown)
  evidence <- forM context (\(Pred c a) -> lift (Unify.substitute fresh a) >>= want c)
  asks envNode >>= mapM_ (`recordEvidence` evidence)
  lift (Unify.substitute fresh t)

-- Classes -------------------------------------------------------------------

newWanted :: Maybe NodeId -> NameId -> Type s -> Tc s (Wanted s)
newWanted n c t = (\hole -> Wanted c t hole n) <$> lift (newSTRef Nothing)

-- | Ask that a type be an instance of a class, for the node being
-- checked. Gives where the evidence will be.
want :: NameId -> Type s -> Tc s (Ev s)
want c t = do
  w <- asks envNode >>= \n -> newWanted n c t
  note refWanted (w :)
  pure (EvHole (wantedHole w))

-- | Do something, and take what it wanted from what is pending.
capture :: Tc s a -> Tc s (a, [Wanted s])
capture inner = do
  ref <- asks refWanted
  outer <- lift (readSTRef ref <* writeSTRef ref [])
  result <- inner
  mine <- lift (readSTRef ref <* writeSTRef ref outer)
  pure (result, reverse mine)

-- | Leave for later, and for whatever is outside.
defer :: [Wanted s] -> Tc s ()
defer ws = note refWanted (reverse ws ++)

-- | What knowing a type is an instance of a class tells: that, and that
-- it is an instance of the class's superclasses, and of theirs.
givenBy :: Program -> NameId -> Type s -> Ev s -> [Given s]
givenBy program c t evidence =
  Given c t evidence
    : concat [givenBy program super t (EvSuper c i evidence) | Just cls <- [classOf program c], (i, super) <- zip [0 ..] (classSupers cls)]

-- | See to what is wanted, as far as the types are known, giving back
-- what must wait: what is wanted of a type still unknown.
--
-- A type built by a constructor is an instance if there is an instance
-- for the constructor, which may want things of its arguments in turn. A
-- type variable is an instance only of what it is given to be.
--
-- An instance that is not there is reported once, however many of the
-- things seen to together want it: a number added to a string wants the
-- string to be a number twice over, for the number and for the adding.
solve :: [Wanted s] -> Tc s [Wanted s]
solve wanted = do
  reported <- lift (newSTRef [])
  solveNoting reported wanted

solveNoting :: STRef s [(NameId, Types.Type)] -> [Wanted s] -> Tc s [Wanted s]
solveNoting reported = fmap concat . mapM one
  where
    -- An instance is there only where imports lead to its module.
    visible w inst = do
      moduleOf <- asks envModuleOf
      reach <- asks envReach
      pure $ case (moduleOf (wantedNode w), moduleOf (Just (instanceId inst))) of
        (Just here, Just there) -> here == there || there `elem` Map.findWithDefault [] here reach
        _ -> True
    one w = do
      (hd, arguments) <- lift (Unify.spine (wantedType w))
      case hd of
        Meta _ -> pure [w]
        Con c ->
          asks (Map.lookup (nameKey (wantedClass w), nameKey c) . envInstances) >>= \case
            Nothing -> [] <$ missing w
            Just inst -> do
              seen <- visible w inst
              if not seen
                then [] <$ missing w
                else do
                  let scope = IntMap.fromList (zip (map nameKey (instanceVariables inst)) arguments)
                  needs <- forM (instanceContext inst) (\(Constraint k t) -> fromSyntaxIn scope t >>= newWanted (wantedNode w) k)
                  lift (writeSTRef (wantedHole w) (Just (EvInstance (instanceId inst) (map (EvHole . wantedHole) needs))))
                  solveNoting reported needs
        Rigid _ -> do
          sought <- lift (Unify.zonk (wantedType w))
          givens <- asks envGivens >>= filterM (\(Given c t _) -> if c == wantedClass w then (== sought) <$> lift (Unify.zonk t) else pure False)
          case givens of
            Given _ _ evidence : _ -> [] <$ lift (writeSTRef (wantedHole w) (Just evidence))
            [] -> [] <$ missing w
        _ -> [] <$ missing w

    missing w = local (\env -> env {envNode = wantedNode w <|> envNode env}) $ do
      program <- asks envProgram
      sought <- (,) (wantedClass w) <$> lift (Unify.zonk (wantedType w))
      before <- lift (readSTRef reported)
      lift (writeSTRef reported (sought : before))
      t <- showType (wantedType w)
      unless (sought `elem` before) $
        complain ("there is no instance of " ++ maybe "?" nameText (nameInfo program (wantedClass w)) ++ " for " ++ t)

-- | See to what is still wanted when everything has been checked. What is
-- wanted of a type nothing settled is settled now: a type that has to be
-- a number is an 'Integer', and any other cannot be told.
settle :: Tc s ()
settle = do
  program <- asks envProgram
  ref <- asks refWanted
  pending <- lift (readSTRef ref <* writeSTRef ref [])
  waiting <- solve (reverse pending)
  let numeric c = case builtinNumeric (programBuiltins program) of
        Just (num, _) -> c == num || any numeric (maybe [] classSupers (classOf program c))
        Nothing -> False
  -- A type that has to be a number is the first of Integer and Double
  -- that is an instance of everything wanted of it.
  instances <- asks envInstances
  let builtins = programBuiltins program
      unknownOf w = lift (Unify.shallow (wantedType w)) >>= \case
        Meta v -> pure (Just v)
        _ -> pure Nothing
  placed <- forM waiting (\w -> (,) w <$> unknownOf w)
  forM_ placed $ \(w, at') -> forM_ at' $ \v -> when (numeric (wantedClass w)) $ do
    still <- lift (Unify.shallow (Meta v))
    case still of
      Meta _ -> do
        -- The types to try are the ones its file's default declaration
        -- names, if it has one.
        let file = fmap originFile (wantedNode w >>= origin program)
        declared' <- forM (lookup file [(Just f, ts) | (f, ts) <- programDefaults program]) (mapM fromSyntax)
        let candidates = fromMaybe [Con (builtinInteger builtins), Con (builtinDouble builtins)] declared'
            wantedOfIt = [wantedClass w' | (w', Just v') <- placed, v' == v]
            fits t = lift (Unify.spine t) >>= \case
              (Con c, _) -> pure (all (\k -> Map.member (nameKey k, nameKey c) instances) wantedOfIt)
              _ -> pure False
        chosen <- filterM fits candidates
        forM_ (take 1 chosen) (lift . Unify.unify still)
      _ -> pure ()
  unsettled <- solve waiting
  forM_ unsettled $ \w -> local (\env -> env {envNode = wantedNode w}) $ do
    let cls = maybe "?" nameText (nameInfo program (wantedClass w))
    complain ("the type of this is ambiguous: nothing says which instance of " ++ cls ++ " is meant")

-- | Check an instance: that its type is an instance of the superclasses,
-- and each method it defines against the method's type at its type.
instance' :: Instance -> Tc s ()
instance' inst = at (instanceId inst) $ do
  program <- asks envProgram
  let self = constructed (instanceType inst) (map Rigid (instanceVariables inst))
  context <- forM (instanceContext inst) (\(Constraint c t) -> Pred c <$> fromSyntax t)
  ds <- forM context (const freshKey)
  recordParameters (instanceId inst) ds
  let givens = concat (zipWith (\(Pred c t) d -> givenBy program c t (EvParameter d)) context ds)
  local (\env -> env {envGivens = givens ++ envGivens env}) $ do
    (supers, wanted) <- capture (forM (maybe [] classSupers (classOf program (instanceClass inst))) (`want` self))
    recordEvidence (instanceId inst) supers
    solve wanted >>= defer
    forM_ (instanceMethods inst) $ \b -> case bindingKind b of
      Function m clauses -> at (bindingId b) $ do
        scheme <- schemeOfName m
        case scheme of
          -- The class's variable is first, and the class first in the
          -- context.
          Poly (a : vs) (_ : own) t -> do
            let scope = IntMap.singleton (nameKey a) self
            t' <- lift (Unify.substitute scope t)
            own' <- forM own (\(Pred c u) -> Pred c <$> lift (Unify.substitute scope u))
            definition (bindingId b) clauses vs own' t'
          _ -> pure ()
      Destructure _ _ -> pure ()

-- Declared types ------------------------------------------------------------

-- | A written type as a type, with its synonyms expanded.
fromSyntax :: Syntax.Type -> Tc s (Type s)
fromSyntax = fromSyntaxIn IntMap.empty

-- | A written type as a type, with types in place of some of its
-- variables.
fromSyntaxIn :: IntMap.IntMap (Type s) -> Syntax.Type -> Tc s (Type s)
fromSyntaxIn = go
  where
    go :: IntMap.IntMap (Type s) -> Syntax.Type -> Tc s (Type s)
    go scope t = at (Syntax.typeId t) $ case spine t [] of
      (Syntax.Con c, arguments) -> do
        arguments' <- mapM (go scope) arguments
        synonym <- asks (IntMap.lookup (nameKey c) . programSynonyms . envProgram)
        case synonym of
          Nothing -> pure (constructed c arguments')
          Just (Synonym parameters body)
            | length arguments' < length parameters -> do
                complain "this type synonym is not given all its arguments"
                unknown
            | otherwise -> do
                let (own, rest) = splitAt (length parameters) arguments'
                expanded <- go (IntMap.fromList (zip (map nameKey parameters) own)) body
                pure (foldl App expanded rest)
      (Syntax.Var a, arguments) -> foldl App (IntMap.findWithDefault (Rigid a) (nameKey a) scope) <$> mapM (go scope) arguments
      (Syntax.Fun a b, []) -> Fun <$> go scope a <*> go scope b
      (Syntax.Fun _ _, _) -> complain "a function type cannot be applied to a type" >> unknown
      (Syntax.App _ _, _) -> unknown
      (Syntax.Error, _) -> unknown

    spine t arguments = case Syntax.typeForm t of
      Syntax.App f a -> spine f (a : arguments)
      form -> (form, arguments)

-- | A type constructor applied to types. The arrow applied to two is a
-- function type.
constructed :: NameId -> [Type s] -> Type s
constructed c arguments = case arguments of
  [a, r] | c == Unify.arrow -> Fun a r
  _ -> foldl App (Con c) arguments

-- | The types of a data type's constructors: from its fields to itself.
constructorSchemes :: DataType -> Tc s [(Int, Poly s)]
constructorSchemes (DataType t parameters constructors) = forM constructors $ \(c, fields) -> do
  fields' <- mapM fromSyntax fields
  pure (nameKey c, Poly parameters [] (foldr Fun (foldl App (Con t) (map Rigid parameters)) fields'))

-- | The type a name was declared to have, if it was. A method's is for
-- the instances of its class.
declared :: NameId -> Tc s (Maybe (Poly s))
declared x = do
  program <- asks envProgram
  forM (signature program x) $ \(Signature vs context t) -> do
    own <- forM context (\(Constraint c u) -> Pred c <$> fromSyntax u)
    let cls = case (nameSort <$> nameInfo program x, vs) of
          (Just (Method c), a : _) -> [Pred c (Rigid a)]
          _ -> []
    Poly vs (cls ++ own) <$> fromSyntax t

-- | The type of a primitive.
primitive :: Prim -> Tc s (Type s)
primitive p = do
  integer <- builtin builtinInteger
  double <- builtin builtinDouble
  char <- builtin builtinChar
  bool <- builtin builtinBool
  unit <- builtin builtinUnit
  io <- builtin builtinIO
  a <- unknown
  b <- unknown
  c <- unknown
  let arithmetic = Fun integer (Fun integer integer)
      comparison = Fun integer (Fun integer bool)
  pure $ case p of
    IntAdd -> arithmetic
    IntSub -> arithmetic
    IntMul -> arithmetic
    IntQuot -> arithmetic
    IntRem -> arithmetic
    IntEq -> comparison
    IntLt -> comparison
    IntLe -> comparison
    Seq -> Fun a (Fun b b)
    MatchFail -> a
    PerformIO -> Fun (App io a) a
    Return -> Fun a (App io a)
    Bind -> Fun (App io a) (Fun (Fun a (App io b)) (App io b))
    PutChar -> Fun integer (Fun char (App io unit))
    GetChar -> App io integer
    Exit -> Fun integer (App io a)
    PathChar -> Fun char (App io unit)
    Throw -> Fun (App io b) (Fun a (App io c))
    Catch -> Fun (App io a) (Fun (Fun b (App io a)) (App io a))
    FileOpen -> Fun integer (App io integer)
    HGetChar -> Fun integer (App io integer)
    FileClose -> Fun integer (App io unit)
    HIsEOF -> Fun integer (App io bool)
    ArgCount -> App io integer
    ArgChar -> Fun integer (Fun integer (App io integer))
    FileSeek -> Fun integer (Fun integer (Fun integer (App io integer)))
    FileTruncate -> Fun integer (Fun integer (App io integer))
    FileFlush -> Fun integer (App io unit)
    IsTerminal -> Fun integer (App io bool)
    EnvChar -> Fun integer (Fun integer (App io integer))
    CellNew -> Fun a (App io integer)
    CellRead -> Fun integer (App io a)
    CellWrite -> Fun integer (Fun a (App io unit))
    CharToInt -> Fun char integer
    IntToChar -> Fun integer char
    Coerce -> Fun a b
    IntNarrow -> Fun integer integer
    FloatNarrow -> Fun double double
    FloatShowAt -> Fun double (Fun integer integer)
    CharCategory -> Fun integer integer
    CharUpper -> Fun integer integer
    CharLower -> Fun integer integer
    CharTitle -> Fun integer integer
    IntToDouble -> Fun integer double
    DoubleAdd -> Fun double (Fun double double)
    DoubleSub -> Fun double (Fun double double)
    DoubleMul -> Fun double (Fun double double)
    DoubleDiv -> Fun double (Fun double double)
    DoublePow -> Fun double (Fun double double)
    DoubleAtan2 -> Fun double (Fun double double)
    DoubleEq -> Fun double (Fun double bool)
    DoubleLt -> Fun double (Fun double bool)
    DoubleLe -> Fun double (Fun double bool)
    DoubleTruncate -> Fun double integer
    DoubleFun -> Fun integer (Fun double double)
    DoubleShowAt -> Fun double (Fun integer integer)
    DoubleMantissa -> Fun double integer
    DoubleExponent -> Fun double integer
    DoubleEncode -> Fun integer (Fun integer double)
    DoubleFromRatio -> Fun integer (Fun integer double)

-- Bindings ------------------------------------------------------------------

-- | Check definitions that are in scope in each other, and then something
-- in their scope.
--
-- A definition with a signature has the type it declares, for everything
-- including itself, and is checked against it. The rest are taken in
-- order of dependency, a group that refers to itself together: within the
-- group each has one type, unknown at first, and when the group is done
-- each is generalised.
bindings :: [Binding] -> Tc s a -> Tc s a
bindings bs inner = do
  let names = concatMap bound bs
  signed <- fmap concat . forM names $ \x -> maybe [] (\s -> [(x, s)]) <$> declared x
  forM_ signed (uncurry recordScheme)
  let unsigned = names \\ map fst signed
      -- A binding is known by the first name it binds, or if it binds none
      -- by a number no name has. Every name it binds leads to it.
      keyed = zipWith (\i b -> (case bound b of x : _ -> nameKey x; [] -> negate i, b)) [1 ..] bs
      aliases = IntMap.fromList [(nameKey x, key) | (key, b) <- keyed, x <- bound b]
      -- Only a use of something with no signature orders the groups.
      uses b = [IntMap.findWithDefault (nameKey x) (nameKey x) aliases | x <- mentions b, x `elem` unsigned]
      groups = map flattenSCC (stronglyConnComp [(b, key, uses b) | (key, b) <- keyed])
  withSchemes signed (foldr group inner groups)

-- | Check a group of definitions that refer to each other, and then
-- something in their scope.
--
-- When the group is done, what it wanted of types that are now free to
-- be anything becomes the context of its functions: each takes a
-- dictionary for each class asked. Unless the monomorphism restriction
-- holds, which is when one of the group is not written as a function:
-- then such a type is left to be settled by how the group is used.
group :: [Binding] -> Tc s a -> Tc s a
group bs inner = do
  env <- ask
  let unsigned = [x | b <- bs, x <- bound b, IntMap.notMember (nameKey x) (envSchemes env)]
      inferring e = e {envInferring = IntSet.union (IntSet.fromList (map nameKey unsigned)) (envInferring e)}
  monos <- forM unsigned (\x -> (,) x <$> unknown)
  ((), wanted) <- capture (local inferring (withMonos monos (mapM_ binding bs)))
  waiting <- solve wanted
  -- A function is generalised. What a pattern binds is not: its one
  -- value is computed once, at one type.
  let functions = [f | b <- bs, Function f _ <- [bindingKind b], f `elem` unsigned]
      restricts b = case bindingKind b of
        Function f clauses -> f `elem` unsigned && f `notElem` envGeneral env && all (null . clausePats) clauses
        Destructure _ _ -> True
      restricted = any restricts bs
      unknowns = fmap concat . mapM (lift . Unify.unsolved)
  held <- unknowns (envMonos env)
  constrained <- unknowns (map wantedType waiting)
  -- What a pattern binds is generalised too, as far as the restriction
  -- lets it be: over what nothing is wanted of.
  candidates <- unknowns (map snd monos)
  let free = [v | v <- nub candidates, v `notElem` held, not (restricted && v `elem` constrained)]
  sorted <- forM waiting (\w -> (,) w . any (`elem` free) <$> lift (Unify.unsolved (wantedType w)))
  let (quantified, deferred) = partition snd sorted
  defer (map fst deferred)
  vs <- forM free $ \v -> do
    a <- NameId <$> freshKey
    a <$ lift (Unify.solve v (Rigid a))
  -- One dictionary for each different thing wanted.
  context <- foldM parameter [] (map fst quantified)
  let ds = [d | (_, _, d) <- context]
      generalised = [(x, Poly vs (if x `elem` functions then [Pred c t | (c, t, _) <- context] else []) t') | (x, t') <- monos]
  forM_ bs $ \b -> case bindingKind b of
    Function f _ | f `elem` functions -> recordParameters (bindingId b) ds
    _ -> pure ()
  -- A use of one of the group inside it passes on what the group takes.
  ref <- asks refUses
  (mine, others) <- partition ((`elem` unsigned) . fst) <$> lift (readSTRef ref)
  lift (writeSTRef ref others)
  forM_ mine (\(x, n) -> when (x `elem` functions) (recordEvidence n (map EvParameter ds)))
  forM_ generalised (uncurry recordScheme)
  local (\e -> e {envMonos = [t | (x, t) <- monos, restricted || x `notElem` functions] ++ envMonos e}) (withSchemes generalised inner)
  where
    parameter context w = do
      t <- lift (Unify.zonk (wantedType w))
      same <- filterM (\(c, u, _) -> if c == wantedClass w then (== t) <$> lift (Unify.zonk u) else pure False) context
      d <- case same of
        (_, _, d) : _ -> pure d
        [] -> freshKey
      lift (writeSTRef (wantedHole w) (Just (EvParameter d)))
      pure (if null same then context ++ [(wantedClass w, wantedType w, d)] else context)

-- | Check a definition whose names are in scope.
binding :: Binding -> Tc s ()
binding b = at (bindingId b) $ case bindingKind b of
  Function f clauses -> do
    Poly vs context t <- schemeOfName f
    inferring <- asks (IntSet.member (nameKey f) . envInferring)
    if inferring
      then do
        record (bindingId b) t
        -- A program's main is an action, whatever it gives.
        program <- asks envProgram
        case nameInfo program f of
          Just (NameInfo "main" Global _) -> do
            action <- App <$> builtin builtinIO <*> unknown
            void (lift (Unify.unify t action))
          _ -> pure ()
        forM_ clauses (`clause` t)
      else do
        definition (bindingId b) clauses vs context t
        -- A program's main is an action, and may not be said to be less.
        program <- asks envProgram
        case nameInfo program f of
          Just (NameInfo "main" Global "Main") -> do
            action <- App <$> builtin builtinIO <*> unknown
            ok <- lift (Unify.unify t action)
            unless ok (complain "main is what a program does, so its type must be IO of something")
          _ -> pure ()
  Destructure p r -> do
    t <- unknown
    record (bindingId b) t
    rhs r t
    binds <- pattern' p t
    -- The names are in scope already, with the types they are to have.
    forM_ binds $ \(x, found) -> do
      Poly _ _ wanted <- schemeOfName x
      at (Pat.patId p) (unify wanted found)

-- | Check the equations of something against the type it is said to
-- have. For each class its context asks it takes a dictionary, and what
-- its equations want is seen to with those.
definition :: NodeId -> [Clause] -> [NameId] -> [Pred s] -> Type s -> Tc s ()
definition n clauses vs context t = do
  record n t
  program <- asks envProgram
  ds <- forM context (const freshKey)
  recordParameters n ds
  let givens = concat (zipWith (\(Pred c a) d -> givenBy program c a (EvParameter d)) context ds)
  local (\env -> env {envGivens = givens ++ envGivens env}) $ do
    ((), wanted) <- capture (forM_ clauses (`clause` t))
    solve wanted >>= defer
  -- A signature's type variables stand for any type at all, so nothing
  -- outside the definition may have been made to depend on one.
  unless (null vs) $ do
    outside <- asks envMonos >>= fmap concat . mapM (lift . Unify.rigids)
    when (any (`elem` vs) outside) $
      complain "this definition is less polymorphic than its signature says: a type variable of the signature has to be the type of something outside it"

-- | Check an equation against the type of its function: each pattern
-- against an argument, and the right-hand side against what is left.
clause :: Clause -> Type s -> Tc s ()
clause c t = at (clauseId c) $ do
  record (clauseId c) t
  go (clausePats c) t
  where
    go [] result = rhs (clauseRhs c) result
    go (p : ps) function = do
      (argument, result) <- unifyFun function
      binds <- pattern' p argument
      withMonos binds (go ps result)

-- | Check a right-hand side against a type: each condition is a Bool, and
-- each body has the type.
rhs :: Rhs -> Type s -> Tc s ()
rhs r t = at (rhsId r) $ do
  record (rhsId r) t
  bindings (rhsWhere r) . forM_ (rhsGuarded r) $ \g -> do
    record (guardedId g) t
    let holds = \case
          [] -> check (guardedBody g) t
          Condition c : rest -> (builtin builtinBool >>= check c) >> holds rest
          Match p e : rest -> do
            binds <- infer e >>= pattern' p
            withMonos binds (holds rest)
          Bindings bs : rest -> bindings bs (holds rest)
    holds (guardedQualifiers g)

-- Patterns ------------------------------------------------------------------

-- | Check a pattern against the type of what it matches, giving the names
-- it binds with their types.
pattern' :: Pat -> Type s -> Tc s [(NameId, Type s)]
pattern' p t = at (Pat.patId p) $ do
  record (Pat.patId p) t
  case Pat.patKind p of
    Pat.Var x -> pure [(x, t)]
    Pat.Wild -> pure []
    Pat.Lit l -> [] <$ (literal True l >>= unify t)
    Pat.Con c ps -> do
      constructor <- schemeOfName c >>= instantiate
      let (fields, result) = arrows constructor
      unify t result
      when (length fields /= length ps) $
        complain ("this constructor has " ++ show (length fields) ++ " fields, but is given " ++ show (length ps) ++ " patterns")
      -- The names in patterns it has no field for are still bound.
      extra <- mapM (const unknown) (drop (length fields) ps)
      concat <$> zipWithM pattern' ps (fields ++ extra)
    Pat.As x q -> ((x, t) :) <$> pattern' q t
    Pat.Lazy q -> pattern' q t
    Pat.Ordered _ q -> pattern' q t
    Pat.Error -> pure []
  where
    arrows = \case
      Fun a b -> let (as, r) = arrows b in (a : as, r)
      r -> ([], r)

-- | The type of a literal, in a pattern or not. A number is of any type
-- that is a number, and to be matched against, one with equality too.
literal :: Bool -> Literal -> Tc s (Type s)
literal matched = \case
  Number _ -> do
    builtins <- asks (programBuiltins . envProgram)
    case (builtinNumeric builtins, builtinEquality builtins) of
      (Just (num, _), equality) | not matched || isJust equality -> do
        a <- unknown
        equal <- if matched then forM (maybeToList equality) (\(eq, _) -> want eq a) else pure []
        number <- want num a
        n <- asks envNode
        forM_ n $ \here -> do
          recordEvidence here (equal ++ [number])
          note refLiterals (here :)
        pure a
      _ -> builtin builtinInteger
  Fraction _ -> do
    builtins <- asks (programBuiltins . envProgram)
    case builtinFractional builtins of
      Just (fractional, _, _) | not matched -> do
        a <- unknown
        evidence <- want fractional a
        asks envNode >>= mapM_ (`recordEvidence` [evidence])
        pure a
      Just (fractional, _, _) | Just (eq, _) <- builtinEquality builtins -> do
        a <- unknown
        equal <- want eq a
        evidence <- want fractional a
        asks envNode >>= mapM_ (`recordEvidence` [equal, evidence])
        pure a
      Just _ -> complain "a number with a fraction needs the prelude's Eq to be matched against" >> unknown
      Nothing -> complain "a number with a fraction needs the prelude's Fractional" >> unknown
  Character _ -> builtin builtinChar
  Text _ -> builtin builtinChar >>= listOf

-- Expressions ---------------------------------------------------------------

-- | Find the type of an expression.
infer :: Expr -> Tc s (Type s)
infer e = at (exprId e) $ do
  t <- case exprKind e of
    HIR.Var x -> do
      inferring <- asks (IntSet.member (nameKey x) . envInferring)
      when inferring (note refUses ((x, exprId e) :))
      schemeOfName x >>= instantiate
    HIR.Con c -> schemeOfName c >>= instantiate
    HIR.Prim p -> primitive p
    HIR.Lit l -> literal False l
    HIR.App f a -> do
      (argument, result) <- infer f >>= unifyFun
      check a argument
      pure result
    HIR.Lam ps body -> do
      arguments <- mapM (const unknown) ps
      binds <- concat <$> zipWithM pattern' ps arguments
      result <- withMonos binds (infer body)
      pure (foldr Fun result arguments)
    HIR.Let bs body -> bindings bs (infer body)
    HIR.If c t f -> do
      builtin builtinBool >>= check c
      result <- infer t
      result <$ check f result
    HIR.Case scrutinee alts -> do
      result <- unknown
      result <$ alternatives scrutinee alts result
    -- What is said is checked as a signature is, and then used as a
    -- name with that signature would be.
    HIR.Annotated inner (Signature vs context said) -> do
      program <- asks envProgram
      preds <- forM context (\(Constraint c u) -> Pred c <$> fromSyntax u)
      t <- fromSyntax said
      ds <- forM preds (const freshKey)
      recordParameters (exprId e) ds
      let givens = concat (zipWith (\(Pred c a) d -> givenBy program c a (EvParameter d)) preds ds)
      local (\env -> env {envGivens = givens ++ envGivens env}) $ do
        ((), wanted) <- capture (check inner t)
        solve wanted >>= defer
      instantiate (Poly vs preds t)
    -- It is whatever it was declared to be.
    HIR.Foreign _ -> unknown
    HIR.Error -> unknown
  t <$ record (exprId e) t

-- | Check an expression against a type it is to have.
--
-- Where the expression has parts whose types follow from its own, they
-- are told theirs in turn: the body of a lambda, the branches of a
-- conditional, the alternatives of a case, the body of a let. Anything
-- else is asked for its type, which is then made the same as the one
-- expected.
check :: Expr -> Type s -> Tc s ()
check e expected = at (exprId e) $ case exprKind e of
  HIR.Lam ps body -> do
    record (exprId e) expected
    let go [] result = check body result
        go (p : rest) function = do
          (argument, result) <- unifyFun function
          binds <- pattern' p argument
          withMonos binds (go rest result)
    go ps expected
  HIR.Let bs body -> record (exprId e) expected >> bindings bs (check body expected)
  HIR.If c t f -> do
    record (exprId e) expected
    builtin builtinBool >>= check c
    check t expected
    check f expected
  HIR.Case scrutinee alts -> record (exprId e) expected >> alternatives scrutinee alts expected
  _ -> infer e >>= unify expected

-- | Check the alternatives of a case: each pattern against the type of
-- what is matched, and each right-hand side against the type of the whole.
alternatives :: Expr -> [Alt] -> Type s -> Tc s ()
alternatives scrutinee alts result = do
  matched <- infer scrutinee
  forM_ alts $ \a -> at (altId a) $ do
    record (altId a) result
    binds <- pattern' (altPat a) matched
    withMonos binds (rhs (altRhs a) result)
