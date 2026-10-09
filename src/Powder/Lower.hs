-- | HIR to Core: pattern-match compilation, guards, and dependency
-- analysis of local bindings.
--
-- Names are settled by now, so this only has to spell them: a top-level
-- name or a constructor as it is written, since definitions link by name,
-- and a local name with its identity after it, so that no two are alike.
module Powder.Lower (lower) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Char (ord)
import Data.Function (on)
import Data.Graph (flattenSCC, stronglyConnComp)
import qualified Data.IntMap.Strict as IntMap
import Control.Applicative ((<|>))
import Data.Functor ((<&>))
import Data.List (nub, nubBy)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Ratio (denominator, numerator)
import qualified Data.Set as Set
import Powder.Core
import Powder.Diagnostics (Problem (..))
import Powder.HIR (NameId (..), NameInfo (..), NameSort (..), NodeId (..), Origin (..))
import qualified Powder.HIR as HIR
import qualified Powder.HIR.Pat as Pat
import qualified Powder.HIR.Type as HIR.Type
import Data.Int (Int64)
import Powder.Typecheck (Checked, evidenceAt, parametersOf, typeOf)
import Powder.Types (Evidence (..))
import qualified Powder.Types as Types

data Env = Env
  { envNames :: HIR.Table NameInfo,
    envOrigins :: HIR.Table Origin,
    -- | Each constructor, by its spelling, with all the constructors of
    -- its type in order.
    envFamilies :: Map.Map Name [ConInfo],
    envProgram :: HIR.Program,
    -- | The dictionaries the type checker found each node to need.
    envChecked :: Checked
  }

-- | An environment, a supply of fresh names, and errors.
type M = ReaderT Env (StateT Int (Either Problem))

lower :: HIR.Program -> Checked -> Either Problem Program
lower program checked = do
  defs <- evalStateT (runReaderT everything env) 0
  -- A later definition of a name replaces an earlier one.
  pure (Program (Map.elems constructors ++ map (dictionary program) classes) (Map.toList (Map.fromList defs)))
  where
    env = Env (HIR.programNames program) (HIR.programOrigins program) families program checked
    classes = IntMap.elems (HIR.programClasses program)
    everything = do
      defined <- dsBinds (HIR.programBindings program)
      ofClasses <- concat <$> mapM dsClass classes
      ofInstances <- mapM dsInstance (HIR.programInstances program)
      pure (defined ++ ofClasses ++ ofInstances)
    infos = IntMap.fromList [(n, info) | (n, NameInfo _ (Constructor info _) _) <- IntMap.toList (HIR.programNames program)]
    constructors = Map.fromList [(conName info, info) | info <- IntMap.elems infos]
    families =
      Map.fromList
        [ (conName info, [infos IntMap.! nameKey c | c <- family])
          | NameInfo _ (Constructor info family) _ <- IntMap.elems (HIR.programNames program)
        ]

-- | Something is wrong at a node.
err :: NodeId -> String -> M a
err (NodeId n) message = do
  from <- asks (IntMap.lookup n . envOrigins)
  lift (lift (Left (Problem "invalid program" message (maybe "" originFile from) (originSpan <$> from) False)))

-- | Fresh names start with @$@, so they cannot clash with names from the
-- source.
fresh :: M Name
fresh = state (\n -> ('$' : show n, n + 1))

nameInfo :: NameId -> M NameInfo
nameInfo (NameId n) = asks (IntMap.lookup n . envNames) >>= maybe (error "Powder.Lower: a name with no entry") pure

-- | How a name is spelled in Core.
name :: NameId -> M Name
name n = do
  info <- nameInfo n
  program <- asks envProgram
  pure $ case nameSort info of
    Local -> nameText info ++ "#" ++ show (nameKey n)
    _ -> spelling program n

conInfo :: NameId -> M ConInfo
conInfo n =
  nameInfo n >>= \case
    NameInfo _ (Constructor c _) _ -> pure c
    NameInfo text _ _ -> error ("Powder.Lower: " ++ text ++ " is not a constructor")

-- | A constructor the lowering needs for itself: the list's, for a string.
wired :: Name -> M ConInfo
wired c = asks (Map.lookup c . envFamilies) >>= \case
  Just family | found : _ <- filter ((== c) . conName) family -> pure found
  _ -> error ("Powder.Lower: no constructor " ++ c)

conFamily :: Name -> M [ConInfo]
conFamily c = asks (Map.findWithDefault [] c . envFamilies)

matchFail :: Core
matchFail = Prim MatchFail

-- Classes -----------------------------------------------------------------

-- A class is a data type with one constructor, whose fields are a
-- dictionary of each superclass and then each method. An instance is a
-- definition that builds one, given the dictionaries its context asks
-- for. A method is the function that takes its field out of one.

-- | How a name declared at the top of a module is spelled: as it is
-- written, with its module's name before it unless that is the root, so
-- that two modules may each have a name of their own.
spelling :: HIR.Program -> NameId -> Name
spelling program n = case HIR.nameInfo program n of
  Just info
    | null (nameModule info) || nameModule info == HIR.programRoot program -> nameText info
    | otherwise -> nameModule info ++ "." ++ nameText info
  Nothing -> "?"

-- | The constructor of a class's dictionaries.
dictionary :: HIR.Program -> HIR.Class -> ConInfo
dictionary program cls = ConInfo ("Dict#" ++ spelling program (HIR.className cls)) 0 (length (HIR.classSupers cls) + length (HIR.classMethods cls)) 1

-- | The field at a place in a dictionary.
field :: ConInfo -> Int -> Core -> Core
field con i d = Case d [Alt con [if k == i then "$field" else "_" | k <- [0 .. conArity con - 1]] (Var "$field")]

-- | How a parameter that is a dictionary is spelled.
parameter :: Int -> Name
parameter d = "dict#" ++ show d

defaultName :: Name -> Name
defaultName m = "default#" ++ m

instanceName :: HIR.Program -> HIR.Instance -> Name
instanceName program inst = "inst#" ++ spelling program (HIR.instanceClass inst) ++ "#" ++ spelling program (HIR.instanceType inst)

-- | The dictionary that evidence says to use.
evidence :: Evidence -> M Core
evidence = \case
  Parameter d -> pure (Var (parameter d))
  Instance n es -> do
    program <- asks envProgram
    case [i | i <- HIR.programInstances program, HIR.instanceId i == n] of
      inst : _ -> foldl App (Var (instanceName program inst)) <$> mapM evidence es
      [] -> pure matchFail
  Superclass c i e -> do
    program <- asks envProgram
    case HIR.classOf program c of
      Just cls -> field (dictionary program cls) i <$> evidence e
      Nothing -> pure matchFail
  Missing -> pure matchFail

-- | Something applied to the dictionaries a node is to be given.
given :: NodeId -> Core -> M Core
given n e = do
  needs <- asks ((`evidenceAt` n) . envChecked)
  foldl App e <$> mapM evidence needs

-- | Something as a function of the dictionaries a node takes.
taking :: NodeId -> Core -> M Core
taking n e = do
  ds <- asks ((`parametersOf` n) . envChecked)
  pure (foldr (Lam . parameter) e ds)

-- | A class's methods, and the definitions they have by default, each a
-- function of a dictionary of the class.
dsClass :: HIR.Class -> M [(Name, Core)]
dsClass cls = do
  program <- asks envProgram
  let con = dictionary program cls
      supers = length (HIR.classSupers cls)
      selectors = [(spelling program m, Lam "$dict" (field con (supers + i) (Var "$dict"))) | (i, m) <- zip [0 ..] (HIR.classMethods cls)]
  defaults <- forM (HIR.classDefaults cls) $ \b -> case HIR.bindingKind b of
    HIR.Function m _ -> pure . (,) (defaultName (spelling program m)) <$> function b
    HIR.Destructure _ _ -> pure []
  pure (selectors ++ concat defaults)

-- | An instance: its dictionary, given those of its context.
dsInstance :: HIR.Instance -> M (Name, Core)
dsInstance inst = do
  program <- asks envProgram
  ds <- asks ((`parametersOf` HIR.instanceId inst) . envChecked)
  let self = foldl App (Var (instanceName program inst)) (map (Var . parameter) ds)
      cls = HIR.classOf program (HIR.instanceClass inst)
      defaulted = [m | Just c <- [cls], b <- HIR.classDefaults c, HIR.Function m _ <- [HIR.bindingKind b]]
  supers <- given (HIR.instanceId inst) (maybe matchFail (Con . dictionary program) cls)
  methods <- forM (maybe [] HIR.classMethods cls) $ \m ->
    case [b | b <- HIR.instanceMethods inst, HIR.Function m' _ <- [HIR.bindingKind b], m' == m] of
      b : _ -> function b
      []
        | m `elem` defaulted -> pure (App (Var (defaultName (spelling program m))) self)
        | otherwise -> pure matchFail
  (,) (instanceName program inst) <$> taking (HIR.instanceId inst) (foldl App supers methods)

mkIf :: Core -> Core -> Core -> Core
mkIf c t e = Case c [Alt falseCon [] e, Alt trueCon [] t]

-- Expressions -------------------------------------------------------------

dsExpr :: HIR.Expr -> M Core
dsExpr e = case HIR.exprKind e of
  HIR.Var x -> name x >>= given (HIR.exprId e) . Var
  -- A newtype's constructor is not there: what it is given is the value.
  HIR.Con c -> do
    erased <- asks ((c `elem`) . HIR.programNewtypes . envProgram)
    if erased then pure (Lam "$x" (Var "$x")) else Con <$> conInfo c
  HIR.Prim p -> pure (Prim p)
  -- A number is made of an integer by its type's way of making one, if
  -- it needs making.
  HIR.Lit (HIR.Number n) -> do
    program <- asks envProgram
    needs <- asks ((`evidenceAt` HIR.exprId e) . envChecked)
    case (needs, HIR.builtinNumeric (HIR.programBuiltins program)) of
      (_ : _, Just (_, fromInteger')) -> (`App` Lit n) <$> given (HIR.exprId e) (Var (spelling program fromInteger'))
      -- A number that is an Int is the one that fits in an Int.
      _ -> do
        checked <- asks envChecked
        pure . Lit $ case typeOf checked (HIR.exprId e) of
          Just (Types.Con c) | c == HIR.builtinInt (HIR.programBuiltins program) -> toInteger (fromInteger n :: Int64)
          _ -> n
  -- A number with a fraction is made of the ratio it is, by its type's
  -- way of making one.
  HIR.Lit (HIR.Fraction q) -> do
    program <- asks envProgram
    case HIR.builtinFractional (HIR.programBuiltins program) of
      Just (_, fromRational', ratio) -> do
        con <- conInfo ratio
        (`App` App (App (Con con) (Lit (numerator q))) (Lit (denominator q))) <$> given (HIR.exprId e) (Var (spelling program fromRational'))
      Nothing -> err (HIR.exprId e) "a number with a fraction needs the prelude"
  HIR.Lit (HIR.Character c) -> pure (Lit (toInteger (ord c)))
  HIR.Lit (HIR.Text s) -> do
    nil <- Con <$> wired "[]"
    cons <- Con <$> wired ":"
    pure (foldr (App . App cons . Lit . toInteger . ord) nil s)
  HIR.App f a -> App <$> dsExpr f <*> dsExpr a
  HIR.Lam ps body -> do
    patterns <- mapM dsPat ps
    dsClauses (length ps) [(patterns, const (dsExpr body))]
  HIR.Let bs body -> dsLet bs (dsExpr body)
  HIR.If c t f -> mkIf <$> dsExpr c <*> dsExpr t <*> dsExpr f
  HIR.Case scrutinee alts -> do
    scrut <- dsExpr scrutinee
    u <- fresh
    clauses <- forM alts $ \a -> (\p -> ([p], dsRhs (HIR.altRhs a))) <$> dsPat (HIR.altPat a)
    Let (NonRec u scrut) <$> match [u] clauses matchFail
  HIR.Annotated inner _ -> dsExpr inner >>= taking (HIR.exprId e) >>= given (HIR.exprId e)
  HIR.Foreign _ -> err (HIR.exprId e) "a C function is only ever what a name is declared to be"
  HIR.Error -> err (HIR.exprId e) "this expression could not be resolved"

-- | Guards fall through to the given failure expression.
dsRhs :: HIR.Rhs -> Core -> M Core
dsRhs r failE = dsLet (HIR.rhsWhere r) (foldr alt (pure failE) (HIR.rhsGuarded r))
  where
    -- A guard that does not hold goes on to the alternatives after it,
    -- which are named once so that each qualifier can fall to them.
    alt g rest = case HIR.guardedQualifiers g of
      [] -> dsExpr (HIR.guardedBody g)
      qs -> rest >>= \next -> withAtom next (\otherwise' -> holds otherwise' qs (dsExpr (HIR.guardedBody g)))
    holds otherwise' qs body = case qs of
      [] -> body
      HIR.Condition c : more -> mkIf <$> dsExpr c <*> holds otherwise' more body <*> pure otherwise'
      HIR.Match p e : more -> do
        scrutinee <- dsExpr e
        u <- fresh
        p' <- dsPat p
        Let (NonRec u scrutinee) <$> match [u] [([p'], const (holds otherwise' more body))] otherwise'
      HIR.Bindings bs : more -> dsLet bs (holds otherwise' more body)

-- Bindings ----------------------------------------------------------------

dsLet :: [HIR.Binding] -> M Core -> M Core
dsLet bs body = do
  binds <- dsBinds bs
  if null binds then body else letRec binds <$> body

-- | Split bindings into strongly connected components, so that only
-- genuinely recursive ones become 'Rec'.
letRec :: [(Name, Core)] -> Core -> Core
letRec binds body = foldr wrap body sccs
  where
    sccs = stronglyConnComp [((x, e), x, Set.toList (freeVars e)) | (x, e) <- binds]
    wrap scc = case flattenSCC scc of
      [(x, e)] | x `Set.notMember` freeVars e -> Let (NonRec x e)
      bs -> Let (Rec bs)

dsBinds :: [HIR.Binding] -> M [(Name, Core)]
dsBinds = fmap concat . mapM binding
  where
    binding b = case HIR.bindingKind b of
      HIR.Function f _ -> do
        x <- name f
        pure . (,) x <$> function b
      -- p = e binds a fresh t to e, and each variable of p to a projection.
      HIR.Destructure p r -> do
        t <- fresh
        p' <- dsPat p
        e <- dsRhs r matchFail
        projections <- forM (patVars p') $ \x ->
          (,) x <$> match [t] [([p'], const (pure (Var x)))] matchFail
        pure ((t, e) : projections)

-- | What a function's equations define: a function of the dictionaries
-- it takes, and then of its arguments.
function :: HIR.Binding -> M Core
function b = case HIR.bindingKind b of
  HIR.Function f [HIR.Clause _ [] (HIR.Rhs _ [HIR.Guarded _ [] (HIR.Expr _ (HIR.Foreign symbol))] [])] -> foreign' (HIR.bindingId b) f symbol
  HIR.Function f equations -> do
    clauses <- forM equations $ \c -> (,dsRhs (HIR.clauseRhs c)) <$> mapM dsPat (HIR.clausePats c)
    case nub (map (length . fst) clauses) of
      [n] -> dsClauses n clauses >>= taking (HIR.bindingId b)
      _ -> do
        x <- name f
        err (HIR.bindingId b) ("equations for " ++ x ++ " have different numbers of arguments")
  HIR.Destructure _ _ -> pure matchFail

-- | A C function as a function here: one that calls it once it has all
-- its arguments, or if it is an action, when the action is run.
foreign' :: NodeId -> NameId -> String -> M Core
foreign' at f symbol = do
  program <- asks envProgram
  let named t = case HIR.Type.typeForm t of
        HIR.Type.Con c -> Just (maybe "" nameText (HIR.nameInfo program c))
        _ -> Nothing
      unwound t = case HIR.Type.typeForm t of
        HIR.Type.Fun a r -> let (as, result) = unwound r in (a : as, result)
        _ -> ([], t)
      floating n
        | n `elem` ["Double", "CDouble"] = Just 'D'
        | n `elem` ["Float", "CFloat"] = Just 'F'
        | otherwise = Nothing
      argument t = named t >>= \n -> floating n <|> (if n `elem` map fst integers then Just 'L' else Nothing)
      resulting t = named t >>= \n -> floating n <|> (if n == "()" then Just 'V' else lookup n integers)
      -- The integers C has, each with how one is read when it comes back.
      integers =
        [ ("Int", 'L'), ("CLong", 'L'), ("CLLong", 'L'), ("Int64", 'L'), ("CULong", 'Q'), ("CULLong", 'Q'), ("CSize", 'Q'),
          ("Word", 'Q'), ("Word64", 'Q'), ("CInt", 'I'), ("Int32", 'I'), ("CUInt", 'U'), ("Word32", 'U'), ("CShort", 's'),
          ("Int16", 's'), ("CUShort", 'S'), ("Word16", 'S'), ("CChar", 'c'), ("CSChar", 'c'), ("Int8", 'c'), ("CUChar", 'C'),
          ("Word8", 'C'), ("Char", 'U')
        ]
  case HIR.signature program f of
    Nothing -> err at "a C function needs its type said"
    Just (HIR.Signature _ _ t) -> do
      let (arguments, final) = unwound t
          io = HIR.builtinIO (HIR.programBuiltins program)
          (action, result) = case HIR.Type.typeForm final of
            HIR.Type.App g x | HIR.Type.Con c <- HIR.Type.typeForm g, c == io -> (True, x)
            _ -> (False, final)
      case (mapM argument arguments, resulting result) of
        (Just shape, Just gives) | length shape <= 3 -> do
          xs <- replicateM (length shape) fresh
          let call = foldl App (Foreign symbol shape gives) (map Var xs)
              -- Running the action is what makes the call, once.
              run' = App (App (Prim Bind) (App (Prim Return) (Lit 0))) (Lam "_" (Let (NonRec "$result" call) (App (App (Prim Seq) (Var "$result")) (App (Prim Return) (Var "$result")))))
          pure (foldr Lam (if action then run' else call) xs)
        _ -> err at "a C function can be called with up to three numbers, and give a number or nothing; this type asks more"

-- Patterns ----------------------------------------------------------------

-- | A pattern as the match compiler sees it: strings are lists and
-- characters are numbers.
data Pattern
  = Variable Name
  | Anything
  | -- | A number, and if it is not just an integer, how to tell whether
    -- something is equal to it.
    Exactly Rational (Maybe Core)
  | Built Name [Pattern]
  | -- | A constructor's pattern, with the places of the fields to match
    -- first.
    Ordered [Int] Pattern
  | Named Name Pattern
  | -- | Matched only when one of its variables is wanted.
    Lazily Pattern

dsPat :: Pat.Pat -> M Pattern
dsPat p = case Pat.patKind p of
  Pat.Var x -> Variable <$> name x
  Pat.Wild -> pure Anything
  Pat.Lit (HIR.Number n) -> do
    program <- asks envProgram
    needs <- asks ((`evidenceAt` Pat.patId p) . envChecked)
    let builtins = HIR.programBuiltins program
    case (needs, HIR.builtinEquality builtins, HIR.builtinNumeric builtins) of
      ([equal, number], Just (_, eq), Just (_, fromInteger')) -> do
        made <- (`App` Lit n) . App (Var (spelling program fromInteger')) <$> evidence number
        Exactly (fromInteger n) . Just . (`App` made) . App (Var (spelling program eq)) <$> evidence equal
      _ -> pure (Exactly (fromInteger n) Nothing)
  Pat.Lit (HIR.Fraction q) -> do
    program <- asks envProgram
    needs <- asks ((`evidenceAt` Pat.patId p) . envChecked)
    let builtins = HIR.programBuiltins program
    case (needs, HIR.builtinEquality builtins, HIR.builtinFractional builtins) of
      ([equal, fractional], Just (_, eq), Just (_, fromRational', ratio)) -> do
        con <- conInfo ratio
        made <- (`App` App (App (Con con) (Lit (numerator q))) (Lit (denominator q))) . App (Var (spelling program fromRational')) <$> evidence fractional
        Exactly q . Just . (`App` made) . App (Var (spelling program eq)) <$> evidence equal
      _ -> err (Pat.patId p) "a number with a fraction cannot be matched against here"
  Pat.Lit (HIR.Character c) -> pure (Exactly (toRational (ord c)) Nothing)
  Pat.Lit (HIR.Text s) -> pure (foldr (\c rest -> Built ":" [Exactly (toRational (ord c)) Nothing, rest]) (Built "[]" []) s)
  Pat.Con c ps -> do
    con <- conInfo c
    erased <- asks ((c `elem`) . HIR.programNewtypes . envProgram)
    unless (length ps == conArity con) $
      err (Pat.patId p) ("constructor " ++ conName con ++ " should have " ++ show (conArity con) ++ " arguments, but has been given " ++ show (length ps))
    -- A newtype's constructor is not there: what is matched is the value.
    case ps of
      [q] | erased -> dsPat q
      _ -> Built (conName con) <$> mapM dsPat ps
  Pat.As x q -> Named <$> name x <*> dsPat q
  Pat.Lazy q -> Lazily <$> dsPat q
  Pat.Ordered order q ->
    dsPat q <&> \case
      built@(Built _ _) -> Ordered order built
      other -> other
  Pat.Error -> err (Pat.patId p) "this pattern could not be resolved"

patVars :: Pattern -> [Name]
patVars = \case
  Variable x -> [x]
  Named x p -> x : patVars p
  Lazily p -> patVars p
  Built _ ps -> concatMap patVars ps
  Ordered _ p -> patVars p
  Anything -> []
  Exactly _ _ -> []

-- Pattern matching --------------------------------------------------------

-- | The patterns still to be matched, and the body, which is given an
-- expression to fall through to when its guards fail.
type Clause = ([Pattern], Core -> M Core)

-- | A function of n arguments defined by clauses.
dsClauses :: Int -> [Clause] -> M Core
dsClauses n clauses = case clauses of
  [(ps, body)] | Just xs <- mapM simple ps -> lams xs <$> body matchFail
  _ -> do
    us <- replicateM n fresh
    lams us <$> match us clauses matchFail
  where
    lams xs e = foldr Lam e xs
    simple = \case Variable x -> Just x; Anything -> Just "_"; _ -> Nothing

-- | What a clause's first pattern is. A constructor's has the places of
-- the fields to match first, where that is not the order they are in.
data Head = HVar | HCon Name [Int] [Pattern] | HLit Rational (Maybe Core)

-- | Wadler's match algorithm ("Efficient compilation of pattern matching",
-- in Peyton Jones 1987). @match us clauses failE@ matches the variables
-- @us@ against the clauses in order, yielding @failE@ if none applies.
--
-- The failure expression is always atomic, so it can be duplicated freely
-- and placed under binders without capture.
match :: [Name] -> [Clause] -> Core -> M Core
match _ [] failE = pure failE
match [] ((_, body) : rest) failE = do
  r <- match [] rest failE
  withAtom r body
match (u : us) clauses failE = go (NE.groupBy ((==) `on` (kind . fst)) (map (headOf u) clauses))
  where
    -- Constructor patterns that take their fields in different orders
    -- are of different kinds: each order is a run of its own.
    kind :: Head -> (Int, [Int])
    kind = \case HVar -> (0, []); HCon _ order _ -> (1, order); HLit {} -> (2, [])

    -- The mixture rule: each run of clauses whose first patterns are of one
    -- kind falls through to the runs after it.
    go [] = pure failE
    go [run] = matchRun (NE.toList run) failE
    go (run : more) = do
      r <- go more
      withAtom r (matchRun (NE.toList run))

    matchRun run f = case run of
      (HCon c order _, _) : _ -> do
        let arrange xs = [x | i <- order, x <- take 1 (drop i xs)] ++ [x | (i, x) <- zip [0 ..] xs, i `notElem` order]
        fam <- conFamily c
        fmap (Case (Var u)) . forM fam $ \ci -> do
          let sub = [(qs, clause) | (HCon c' _ qs, clause) <- run, c' == conName ci]
          if null sub
            then pure (Alt ci (replicate (conArity ci) "_") f)
            else do
              vs <- replicateM (conArity ci) fresh
              Alt ci vs <$> match (arrange vs ++ us) [(arrange qs ++ ps, body) | (qs, (ps, body)) <- sub] f
      (HLit _ _, _) : _ -> foldr test (pure f) (nubBy ((==) `on` fst) [(n, how) | (HLit n how, _) <- run])
        where
          test (n, how) rest =
            mkIf (maybe (App (App (Prim IntEq) (Var u)) (Lit (numerator n))) (`App` Var u) how)
              <$> match us [clause | (HLit n' _, clause) <- run, n' == n] f
              <*> rest
      _ -> match us (map snd run) f

-- | Take the first pattern off a clause, binding the variables it names to
-- the scrutinee.
headOf :: Name -> Clause -> (Head, Clause)
headOf u (pats, body) = case pats of
  Anything : ps -> (HVar, (ps, body))
  Variable x : ps -> (HVar, (ps, bound x body))
  Named x p : ps -> let (h, (ps', body')) = headOf u (p : ps, body) in (h, (ps', bound x body'))
  Built c qs : ps -> (HCon c [] qs, (ps, body))
  Ordered order (Built c qs) : ps -> (HCon c order qs, (ps, body))
  Ordered _ p : ps -> headOf u (p : ps, body)
  -- Each variable is what matching would bind it to, if it comes to that.
  Lazily p : ps ->
    ( HVar,
      ( ps,
        \f -> do
          projections <- forM (patVars p) (\x -> (,) x <$> match [u] [([p], const (pure (Var x)))] matchFail)
          inner <- body f
          pure (foldr (\(x, e) -> Let (NonRec x e)) inner projections)
      )
    )
  Exactly n how : ps -> (HLit n how, (ps, body))
  [] -> error "headOf: clause has no patterns left"
  where
    bound x b f = Let (NonRec x (Var u)) <$> b f

withAtom :: Core -> (Core -> M Core) -> M Core
withAtom e k = case e of
  Var _ -> k e
  Lit _ -> k e
  Prim _ -> k e
  _ -> do
    v <- fresh
    Let (NonRec v e) <$> k (Var v)
