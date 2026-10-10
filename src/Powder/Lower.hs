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
import Data.Bifunctor (second)
import Data.Functor ((<&>))
import Data.List (isPrefixOf, nub, nubBy)
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
type M = ReaderT Env (StateT (Int, [Export]) (Either Problem))

lower :: HIR.Program -> Checked -> Either Problem Program
lower program checked = do
  (defs, (_, exports)) <- runStateT (runReaderT everything env) (0, [])
  -- A later definition of a name replaces an earlier one.
  pure (Program (Map.elems constructors ++ map (dictionary program) classes) (Map.toList (Map.fromList defs)) (reverse exports))
  where
    env = Env (HIR.programNames program) (HIR.programOrigins program) families program checked
    classes = IntMap.elems (HIR.programClasses program)
    everything = do
      defined <- dsBinds (HIR.programBindings program)
      -- Another package's classes and instances are that package's to make.
      ofClasses <- concat <$> mapM dsClass [c | c <- classes, HIR.mine program (nameKey (HIR.className c))]
      ofInstances <- mapM dsInstance [i | i <- HIR.programInstances program, HIR.mine program (HIR.nodeKey (HIR.instanceId i))]
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
fresh = state (\(n, exports) -> ('$' : show n, (n + 1, exports)))

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
function b = asks envProgram >>= \whole -> case HIR.bindingKind b of
  HIR.Function f [HIR.Clause _ [] (HIR.Rhs _ [HIR.Guarded _ [] (HIR.Expr _ (HIR.Foreign symbol))] [])] -> foreign' (HIR.bindingId b) f symbol
  -- The definition a foreign export makes, of the function it exports.
  HIR.Function f [HIR.Clause _ [] rhs] | Just (NameInfo text _ _) <- HIR.nameInfo whole f, "export#" `isPrefixOf` text -> do
    exported <- dsClauses 0 [([], dsRhs rhs)] >>= taking (HIR.bindingId b)
    export (HIR.bindingId b) f (drop 7 text) exported
  HIR.Function f equations -> do
    clauses <- forM equations $ \c -> (,dsRhs (HIR.clauseRhs c)) <$> mapM dsPat (HIR.clausePats c)
    case nub (map (length . fst) clauses) of
      [n] -> dsClauses n clauses >>= taking (HIR.bindingId b)
      _ -> do
        x <- name f
        err (HIR.bindingId b) ("equations for " ++ x ++ " have different numbers of arguments")
  HIR.Destructure _ _ -> pure matchFail

-- | What crosses between the program and C is numbers: integers of C's
-- sizes, floats and doubles, and addresses. Each type of a function that
-- is C's, or is made C's, is one of those, by a letter the machines know:
-- a type the libraries have for the purpose, or a newtype or a synonym
-- of one. A 'Bool' crosses as 0 or 1, and a 'Char' as its code.
data Crossing = Crossing
  { -- | The letter of a type, if it is one that crosses.
    crossingKind :: HIR.Type.Type -> Maybe Char,
    -- | The types of a function's arguments, whether what it gives is an
    -- action, the type of what that gives, and its letter, which for
    -- nothing is @V@.
    crossingSplit :: HIR.Type.Type -> ([HIR.Type.Type], Bool, HIR.Type.Type, Maybe Char),
    -- | A value as the number that crosses, given its type and letter;
    -- and the number as the value.
    crossingOut :: HIR.Type.Type -> Char -> Core -> Core,
    crossingBack :: HIR.Type.Type -> Core -> Core,
    -- | What a type is of, if it is a type applied to one other.
    crossingOf :: HIR.Type.Type -> Maybe HIR.Type.Type
  }

crossing :: HIR.Program -> Crossing
crossing program = Crossing (kindOf 8) split out back argumentOf
  where
    -- A type with the synonyms that stand for whole types written out,
    -- so that one for a function's type is seen to be a function's.
    plain t = case HIR.Type.typeForm t of
      HIR.Type.Con c | Just (HIR.Synonym [] meant) <- IntMap.lookup (nameKey c) (HIR.programSynonyms program) -> plain meant
      HIR.Type.App g x -> t {HIR.Type.typeForm = HIR.Type.App (plain g) (plain x)}
      HIR.Type.Fun a r -> t {HIR.Type.typeForm = HIR.Type.Fun (plain a) (plain r)}
      _ -> t
    unwound t = case HIR.Type.typeForm t of
      HIR.Type.Fun a r -> let (as, result) = unwound r in (a : as, result)
      _ -> ([], t)
    constructorOf t = case HIR.Type.typeForm t of
      HIR.Type.Con c -> Just c
      HIR.Type.App g _ -> constructorOf g
      _ -> Nothing
    argumentOf t = case HIR.Type.typeForm t of
      HIR.Type.App _ x -> Just x
      _ -> Nothing
    -- The language's own types and the libraries' are known by name. Any
    -- other is a newtype of something that is.
    ours c = case HIR.nameInfo program c of
      Just info | null (nameModule info) || any (`isPrefixOf` nameModule info) ["Powder.", "Data.", "Foreign."] -> lookup (nameText info) kinds
      _ -> Nothing
    kindOf :: Int -> HIR.Type.Type -> Maybe Char
    kindOf depth t = do
      c <- constructorOf t
      ours c <|> case [inner | depth > 0, d <- HIR.programDataTypes program, HIR.dataName d == c, [(k, [inner])] <- [HIR.dataConstructors d], k `elem` HIR.programNewtypes program] of
        inner : _ -> kindOf (depth - 1) inner
        -- A synonym is what it stands for.
        [] | depth > 0 -> IntMap.lookup (nameKey c) (HIR.programSynonyms program) >>= kindOf (depth - 1) . HIR.synonymType
        [] -> Nothing
    isUnit t = fmap (== HIR.builtinUnit (HIR.programBuiltins program)) (constructorOf t) == Just True
    isBool t = fmap (== HIR.builtinBool (HIR.programBuiltins program)) (constructorOf t) == Just True
    kinds =
      [ ("Int", 'L'), ("Word", 'Q'), ("Int8", 'c'), ("Int16", 's'), ("Int32", 'I'), ("Int64", 'L'), ("Word8", 'C'), ("Word16", 'S'),
        ("Word32", 'U'), ("Word64", 'Q'), ("Char", 'U'), ("Bool", 'C'), ("Double", 'D'), ("Float", 'F'), ("Ptr", 'P'), ("FunPtr", 'P'),
        ("StablePtr", 'P'), ("IntPtr", 'L'), ("WordPtr", 'Q'), ("CChar", 'c'), ("CSChar", 'c'), ("CUChar", 'C'), ("CShort", 's'),
        ("CUShort", 'S'), ("CInt", 'I'), ("CUInt", 'U'), ("CLong", 'L'), ("CULong", 'Q'), ("CLLong", 'L'), ("CULLong", 'Q'),
        ("CSize", 'Q'), ("CPtrdiff", 'L'), ("CWchar", 'I'), ("CSigAtomic", 'I'), ("CClock", 'L'), ("CTime", 'L'), ("CFloat", 'F'),
        ("CDouble", 'D'), ("CIntPtr", 'L'), ("CUIntPtr", 'Q'), ("CIntMax", 'L'), ("CUIntMax", 'Q')
      ]
    split written =
      let (arguments, final) = unwound (plain written)
          io = HIR.builtinIO (HIR.programBuiltins program)
          (action, result) = case HIR.Type.typeForm final of
            HIR.Type.App g x | HIR.Type.Con c <- HIR.Type.typeForm g, c == io -> (True, x)
            _ -> (False, final)
       in (arguments, action, result, if isUnit result then Just 'V' else kindOf 8 result)
    out t kind e
      | isBool t = Case e [Alt falseCon [] (Lit 0), Alt trueCon [] (Lit 1)]
      | kind `elem` "DF" = e
      | otherwise = App (Prim IntNarrow) e
    back t e
      | isBool t = Case (App (App (Prim IntEq) e) (Lit 0)) [Alt falseCon [] (Con trueCon), Alt trueCon [] (Con falseCon)]
      | otherwise = e

unfit :: String
unfit = "a C function takes and gives numbers and addresses: the types the Foreign libraries have for them, or newtypes of those"

-- | A C function as a function here: one that calls it once it has all
-- its arguments, or if it is an action, when the action is run.
foreign' :: NodeId -> NameId -> String -> M Core
foreign' at f symbol = do
  program <- asks envProgram
  let cross = crossing program
      -- Running the action is what makes the call, once.
      once call = App (App (Prim Bind) (App (Prim Return) (Lit 0))) (Lam "_" (Let (NonRec "$result" call) (App (App (Prim Seq) (Var "$result")) (App (Prim Return) (Var "$result")))))
  case HIR.signature program f of
    Nothing -> err at "a C function needs its type said"
    Just (HIR.Signature _ _ t) -> do
      let (arguments, action, result, gives) = crossingSplit cross t
      case (symbol, mapM (crossingKind cross) arguments, gives) of
        -- A C function made of one of the program's: the address of
        -- something that, called, has the action that answers run.
        ("wrapper", _, _) -> case (arguments, action, crossingOf cross result) of
          ([taken], True, Just made) | crossingKind cross result == Just 'P' && sameType taken made -> do
            x <- fresh
            (shape, kind, answer) <- answering at taken (Var x)
            pure (Lam x (once (App (Foreign symbol shape kind) answer)))
          _ -> err at "a C function made from a function has the type `f -> IO (FunPtr f)`, for a function type f"
        ('&' : _, Just [], Just 'P') | not action -> pure (Foreign symbol "" 'P')
        ('&' : _, _, _) -> err at "the address of something of C's is a Ptr or a FunPtr, and nothing else"
        ("dynamic", Just shape, _) | take 1 shape /= "P" -> err at "a C function called by its address takes the address, a FunPtr, first"
        (_, Just shape, Just kind) -> do
          xs <- replicateM (length shape) fresh
          let call = crossingBack cross result (foldl App (Foreign symbol shape kind) (zipWith3 (crossingOut cross) arguments shape (map Var xs)))
          pure (foldr Lam (if action then once call else call) xs)
        _ -> err at unfit
  where
    -- Whether two types are written the same.
    sameType a b = case (HIR.Type.typeForm a, HIR.Type.typeForm b) of
      (HIR.Type.Con c, HIR.Type.Con d) -> c == d
      (HIR.Type.Var c, HIR.Type.Var d) -> c == d
      (HIR.Type.App g x, HIR.Type.App h y) -> sameType g h && sameType x y
      (HIR.Type.Fun x r, HIR.Type.Fun y s) -> sameType x y && sameType r s
      _ -> False

-- | The action that answers a call from C of a function of the
-- program's, given the type C sees the function at: it takes the
-- arguments of the call, gives them to the function, and gives back what
-- comes of that. With it are the letters of the arguments and of the
-- result.
answering :: NodeId -> HIR.Type.Type -> Core -> M (String, Char, Core)
answering at t called = do
  cross <- asks (crossing . envProgram)
  let (arguments, action, result, gives) = crossingSplit cross t
  case (mapM (crossingKind cross) arguments, gives) of
    (Just shape, Just kind) -> do
      xs <- replicateM (length shape) fresh
      r <- fresh
      let bind m x k = App (App (Prim Bind) m) (Lam x k)
          real k = k `elem` "DF"
          taken i k = App (Prim (if real k then CallbackDouble else CallbackInt)) (Lit i)
          applied = foldl App called (zipWith (crossingBack cross) arguments (map Var xs))
          answered v
            | kind == 'V' = App (Prim Return) (Lit 0)
            | otherwise = App (Prim (if real kind then CallbackReturnDouble else CallbackReturnInt)) (crossingOut cross result kind v)
          -- What is not an action is worked out when C asks.
          body = if action then bind applied r (answered (Var r)) else Let (NonRec r applied) (App (App (Prim Seq) (Var r)) (answered (Var r)))
      pure (shape, kind, foldr (\(x, i, k) rest -> bind (taken i k) x rest) body (zip3 xs [0 ..] shape))
    _ -> err at unfit

-- | The definition a foreign export makes: the action that answers C's
-- call of the function, which is noted as one C may call.
export :: NodeId -> NameId -> String -> Core -> M Core
export at f called exported = do
  program <- asks envProgram
  case HIR.signature program f of
    Nothing -> err at "a function made C's needs its type said"
    Just (HIR.Signature _ _ t) -> do
      (shape, kind, answer) <- answering at t exported
      modify (second (Export called shape kind (spelling program f) :))
      pure answer

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
