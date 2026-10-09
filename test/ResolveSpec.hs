-- | Name resolution: what HIR says about a program's names and nodes.
module ResolveSpec (spec) where

import Control.Monad (forM_)
import qualified Data.IntMap.Strict as IntMap
import Data.List (isInfixOf, nub, sort)
import Data.Maybe (isJust)
import Powder.Diagnostics (Failure (..), Problem (..))
import Powder.Frontend
import Powder.HIR
import qualified Powder.HIR.Pat as Pat
import Powder.Syntax.Token (Pos (..))
import Test.Hspec

spec :: Spec
spec = forM_ tests (\(name, ok) -> it name (ok `shouldBe` True))

-- | A few definitions for sources to lean on, as the prelude would give.
base :: String
base = "type Int = Integer\nnegate x = primIntSub 0 x\nenumFrom n = n\nenumFromTo a b = a\nm >>= k = primBind m k\nx + y = primIntAdd x y\nx * y = primIntMul x y\ninfixl 6 +\ninfixl 7 *\ndata Maybe a = Nothing | Just a\n"

-- | Resolve a source on top of the base.
hir :: String -> Either Failure Program
hir src = do
  b <- parseFile "base" base
  u <- parseFile "test" src
  resolved [b, u] Nothing

-- | The program, when nothing is wrong with the source.
good :: String -> Program
good src = either (error . unlines . map problemMessage . failureProblems) id (hir src)

-- | What is wrong with a source, each with the line and column it is at.
bad :: String -> [(String, Maybe Pos)]
bad src = either (\f -> [(problemMessage p, fst <$> problemSpan p) | p <- failureProblems f]) (const []) (hir src)

-- | A source's definitions, as text, without the base's.
shown :: String -> [String]
shown src = lines (pretty p {programBindings = filter own (programBindings p)})
  where
    p = good src
    own b = fmap originFile (origin p (bindingId b)) == Just "test"

-- | The text with each name's identity taken off, to compare shapes.
plain :: String -> String
plain = \case
  '#' : rest -> plain (dropWhile (`elem` ['0' .. '9']) rest)
  c : rest -> c : plain rest
  [] -> []

-- | The identities after each use of a spelling, in order.
idsOf :: String -> String -> [String]
idsOf spelling = go
  where
    marker = spelling ++ "#"
    go s
      | null s = []
      | marker `isPrefix` s = takeWhile (`elem` ['0' .. '9']) (drop (length marker) s) : go (drop (length marker) s)
      | otherwise = go (drop 1 s)
    isPrefix a b = take (length a) b == a

tests :: [(String, Bool)]
tests =
  [ -- Scope.
    ("a use is the identity of its binder", let ids = idsOf "x" (unlines (shown "f x = x")) in length ids == 2 && length (nub ids) == 1),
    ("a shadowing binder is a different name", let ids = idsOf "x" (unlines (shown "f x = \\x -> x")) in length (nub ids) == 2 && ids !! 1 == ids !! 2),
    ("and the outer name is the same before and after", let ids = idsOf "x" (unlines (shown "f x = g ((\\x -> x) x) x\ng a b = a")) in take 1 ids == take 1 (reverse ids) && take 1 (drop 1 ids) /= take 1 ids),
    ("let bindings see each other", null (bad "f = let { a = b; b = 1 } in a")),
    ("and themselves", null (bad "f = let xs = xs in xs")),
    ("where is in scope in guards and bodies", null (bad "f x\n  | c = y\n  | d = 0\n  where\n    c = x\n    d = c\n    y = 1")),
    ("a where does not reach the next equation", map fst (bad "f 0 = y where y = 1\nf n = y") == ["variable not in scope: y"]),
    ("a lambda's names end with it", map fst (bad "f = (\\x -> x) x") == ["variable not in scope: x"]),
    ("a case alternative's names are its own", map fst (bad "f m = case m of { Just a -> a; Nothing -> a }") == ["variable not in scope: a"]),
    ("a do block binds for what follows", null (bad "f g = do { x <- g; let { y = x }; g y }") && map fst (bad "f g = do { g x; x <- g; g x }") == ["variable not in scope: x"]),
    ("top-level definitions see each other in any order", null (bad "f = g\ng = f")),
    ("a local name hides a global", let ids = idsOf "g" (unlines (shown "g = 1\nf g = g")) in length (nub ids) == 2),
    ("a later module's definition replaces an earlier one", length [() | b <- programBindings (good "negate x = x"), Function f _ <- [bindingKind b], fmap nameText (nameInfo (good "negate x = x") f) == Just "negate"] == 1),
    ("for the earlier module's uses too", either (const False) (const True) (hir "x + y = x")),
    -- What is wrong.
    ("an unknown variable is reported where it is", bad "f = 1\ng = f + oops" == [("variable not in scope: oops", Just (Pos 2 9))]),
    ("every one of them, not only the first", map fst (bad "f = a + b\ng = c") == ["variable not in scope: a", "variable not in scope: b", "variable not in scope: c"]),
    ("an unknown constructor", bad "f (Nope x) = Other x" == [("unknown constructor Nope", Just (Pos 1 4)), ("unknown constructor Other", Just (Pos 1 14))]),
    ("a name defined twice", bad "f = 1\ng = 2\nf = 3" == [("conflicting definitions for f", Just (Pos 3 1))]),
    ("or twice in a let", map fst (bad "f = let { a = 1; b = 2; a = 3 } in a") == ["conflicting definitions for a"]),
    ("adjacent equations are one definition", null (bad "f 0 = 1\nf n = n")),
    ("a name bound twice in one set of patterns", map fst (bad "f x x = x") == ["x is bound more than once in these patterns"]),
    ("a do block that ends in a binding", map fst (bad "f g = do { x <- g }") == ["the last statement in a 'do' block must be an expression"]),
    ("a floating-point literal is the ratio it is", either (const False) (const True) (hir "x = 1.5")),
    -- The little desugaring.
    ("operators become applications, by precedence", map plain (shown "f a b c = a + b * c") == ["f a b c = + a (* b c)"]),
    ("and by associativity", map plain (shown "f a b c = a + b + c") == ["f a b c = + (+ a b) c"]),
    ("fixities declared later still hold", map plain (shown "f a b c = a <> b <> c\ninfixr 5 <>\na <> b = a") == ["f a b c = <> a (<> b c)", "<> a b = a"]),
    ("a minus sign is negate, and binds less than times", map plain (shown "f a b = - a * b") == ["f a b = negate (* a b)"]),
    ("a negative number is the negation of a number", map plain (shown "f = -1") == ["f = negate 1"]),
    ("sections are applications and lambdas", map plain (shown "f = (1 +)\ng = (+ 1)") == ["f = + 1", "g = \\x -> + x 1"]),
    ("brackets go", map plain (shown "f a = ((a))") == ["f a = a"]),
    ("tuples, lists and the unit are constructors", map plain (shown "f a = (a, [a, a], ())") == ["f a = (,,) a (: a (: a [])) ()"]),
    ("in patterns too", map plain (shown "f (a, [b], ()) = a") == ["f ((,,) a (: b []) ()) = a"]),
    ("ranges", map plain (shown "f a b = [a ..]\ng a b = [a .. b]") == ["f a b = enumFrom a", "g a b = enumFromTo a b"]),
    ("do is bind and lambda", map plain (shown "f g h = do { x <- g; h; h x }") == ["f g h = >>= g (\\x -> >>= h (\\_ -> h x))"]),
    ("the equations of a function are one binding", length (programBindings' "f 0 = 1\nf n = n\ng = f") == 2),
    ("primitives are primitives, unless hidden", map plain (shown "f = primIntAdd\ng primIntAdd = primIntAdd") == ["f = primIntAdd", "g primIntAdd = primIntAdd"] && null (idsOf "primIntAdd" (unlines (shown "f = primIntAdd")))),
    ("what the sugar leans on is the global, whatever is in scope", let ids = idsOf "negate" (unlines (shown "f negate x = - x")) in length ids == 2 && length (nub ids) == 2),
    -- Identities and tables.
    ("no two nodes share an identity", let ns = nodeIds (good sample) in length ns == length (nub ns)),
    ("every node has an origin", let p = good sample in all (isJust . origin p) (nodeIds p)),
    ("an origin is where the node was written", let p = good "f = 1\ng =  f" in [originSpan o | b <- programBindings p, Function g [c] <- [bindingKind b], fmap nameText (nameInfo p g) == Just "g", [gd] <- [rhsGuarded (clauseRhs c)], Just o <- [origin p (exprId (guardedBody gd))]] == [(Pos 2 6, Pos 2 7)]),
    ("every name used has an entry", let p = good sample in all (isJust . nameInfo p) (namesIn p)),
    ("a table keyed by node holds one entry a node", let p = good sample; table = IntMap.fromList [(nodeKey n, ()) | n <- nodeIds p] in IntMap.size table == length (nodeIds p)),
    ("constructors know their type's others", let p = good "data T = A | B Int | C" in sort [length family | NameInfo c (Constructor _ family) _ <- IntMap.elems (programNames p), c `elem` ["A", "B", "C"]] == [3, 3, 3]),
    ("names are global, local or constructors", let p = good "data T = A\nf x = A" in nub (sort [(nameText i, sortOf i) | i <- IntMap.elems (programNames p), nameText i `elem` ["A", "f", "x"]]) == [("A", "constructor"), ("f", "global"), ("x", "local")]),
    ("the expression a prompt asks for is a binding of its own", either (const False) (\p -> "$it" `elem` [nameText i | b <- programBindings p, n <- bound b, Just i <- [nameInfo p n]]) (asked "1 + 2")),
    -- Modules.
    ("a module sees what it imports", fine [("A", "module A where\nx = 1"), ("B", "module B where\nimport A\ny = x")]),
    ("but not what is not exported", wrongIn [("A", "module A (x) where\nx = 1\nz = 2"), ("B", "module B where\nimport A\ny = z")] == ["variable not in scope: z"]),
    ("an import that lists takes only those", wrongIn [("A", "module A where\nx = 1\nz = 2"), ("B", "module B where\nimport A (x)\ny = z")] == ["variable not in scope: z"]),
    ("an import that hides takes the rest", wrongIn [("A", "module A where\nx = 1\nz = 2"), ("B", "module B where\nimport A hiding (x)\ny = (x, z)")] == ["variable not in scope: x"]),
    ("a qualified import gives only qualified names", wrongIn [("A", "module A where\nx = 1"), ("B", "module B where\nimport qualified A\ny = (A.x, x)")] == ["variable not in scope: x"]),
    ("a module may be renamed", fine [("A", "module A where\nx = 1"), ("B", "module B where\nimport qualified A as Q\ny = Q.x")]),
    ("a type comes with the constructors asked for", wrongIn [("A", "module A where\ndata T = K | L"), ("B", "module B where\nimport A (T (K))\ny = (K, L)")] == ["unknown constructor L"]),
    ("modules may import each other", fine [("A", "module A where\nimport B\nx = y"), ("B", "module B where\nimport A\ny = x")]),
    ("and export each other's names on", fine [("A", "module A (module B, x) where\nimport B\nx = 1"), ("B", "module B (module A, y) where\nimport A\ny = x"), ("C", "module C where\nimport A\nz = (x, y)")]),
    ("a name two imports give is ambiguous", wrongIn [("A", "module A where\nx = 1"), ("B", "module B where\nx = 2"), ("C", "module C where\nimport A\nimport B\ny = x")] == ["x is ambiguous: more than one of the modules imported here exports it"]),
    ("unless it is the same thing both ways", fine [("A", "module A where\nx = 1"), ("B", "module B (x) where\nimport A"), ("C", "module C where\nimport A\nimport B\ny = x")]),
    ("a module's own name and an imported one are ambiguous", wrongIn [("A", "module A where\nx = 1"), ("B", "module B where\nimport A\nx = 2\ny = x")] == ["x is ambiguous: more than one of the modules imported here exports it"]),
    ("unless it is said which", fine [("A", "module A where\nx = 1"), ("B", "module B where\nimport A\nx = 2\ny = B.x + A.x\nm + n = m")]),
    ("or the import hides it", fine [("A", "module A where\nx = 1"), ("B", "module B where\nimport A hiding (x)\nx = 2\ny = x")]),
    ("a fixity goes with the operator it is declared for", (shownIn [("A", "module A where\ninfixr 5 +++\na +++ b = a"), ("B", "module B where\nimport A\ny = 1 +++ 2 +++ 3")], shownIn [("A", "module A where\ninfixr 5 +++\na +++ b = a"), ("B", "module B where\nimport qualified A\na +++ b = b\ny = 1 +++ 2 +++ 3")]) == (True, False)),
    ("a fixity for what is not defined there", wrongIn [("A", "module A where\ninfixl 5 +++\nx = 1")] == ["a fixity is given for +++, which is not defined here"]),
    ("a module that is not there", wrongIn [("B", "module B where\nimport A\ny = 1")] == ["there is no module A"]),
    ("an import of what is not exported", wrongIn [("A", "module A (x) where\nx = 1\nz = 2"), ("B", "module B where\nimport A (z)")] == ["A does not export z"]),
    ("an export of what is not in scope", wrongIn [("A", "module A (nope) where\nx = 1")] == ["nope is exported, but is not in scope"]),
    ("two modules may each have a name", fine [("A", "module A where\nx = 1"), ("B", "module B where\nimport qualified A\nx = A.x")]),
    ("the pretty printer shows identities", any ("#" `isInfixOf`) (shown "f x = x"))
  ]
  where
    package files = mapM (uncurry parseFile) files >>= (`resolved` Nothing)
    fine = either (const False) (const True) . package
    wrongIn = either (map problemMessage . failureProblems) (const []) . package
    -- Whether y groups its operators to the right: its last argument is in parentheses.
    shownIn files = either (const False) (any (\l -> take 2 l == "y#" && take 1 (reverse l) == ")") . lines . pretty) (package files)
    sample = "data T = A | B Int\nf (B n) y | n = [y, 1] | y = let { z = n } in (z, \"s\")\nf A _ = case A of { B k -> k; _ -> 'c' }\n  where w = 1\n(p, q) = f A 2"
    programBindings' src = filter (\b -> fmap originFile (origin (good src) (bindingId b)) == Just "test") (programBindings (good src))
    sortOf i = case nameSort i of
      Global -> "global"
      Local -> "local"
      Constructor _ _ -> "constructor"
      _ -> "type"
    asked src = do
      b <- parseFile "base" base
      parseLine "entry" src >>= \case
        Expression e -> resolved [b] (Just (Asked "$it" Nothing e))
        Declarations _ -> Left (Failure [] [])

-- | Every name a program's nodes mention.
namesIn :: Program -> [NameId]
namesIn = concatMap binding . programBindings
  where
    binding b = case bindingKind b of
      Function f clauses -> f : concatMap clause clauses
      Destructure p r -> pat p ++ rhs r
    clause c = concatMap pat (clausePats c) ++ rhs (clauseRhs c)
    rhs r = concatMap guarded (rhsGuarded r) ++ concatMap binding (rhsWhere r)
    guarded g = concatMap qualifier (guardedQualifiers g) ++ expr (guardedBody g)
    qualifier = \case
      Condition c -> expr c
      Match p e -> pat p ++ expr e
      Bindings bs -> concatMap binding bs
    pat p = case Pat.patKind p of
      Pat.Var x -> [x]
      Pat.Con c ps -> c : concatMap pat ps
      Pat.As x q -> x : pat q
      _ -> []
    expr e = case exprKind e of
      Var x -> [x]
      Con c -> [c]
      App f a -> expr f ++ expr a
      Lam ps body -> concatMap pat ps ++ expr body
      Let bs body -> concatMap binding bs ++ expr body
      If c t f -> expr c ++ expr t ++ expr f
      Case s alts -> expr s ++ concat [pat (altPat a) ++ rhs (altRhs a) | a <- alts]
      _ -> []
