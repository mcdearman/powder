-- | The type checker: the types it finds, and what it finds wrong.
module TypecheckSpec (spec) where

import Control.Monad (forM_)
import qualified Data.IntMap.Strict as IntMap
import Data.List (isInfixOf, sort)
import Data.Maybe (isJust)
import Powder.Diagnostics (Failure (..), Problem (..))
import Powder.Frontend
import Powder.HIR
import Powder.Syntax.Token (Pos (..))
import Powder.Typecheck
import Powder.Types (prettyScheme, prettyType)
import System.Directory (listDirectory)
import Test.Hspec

spec :: Spec
spec = do
  prelude <- runIO (either (error "the prelude does not parse") id <$> loadBase)
  examples <- runIO (sort . filter (".hs" `isInfixOf`) <$> listDirectory "examples")
  -- Each with the modules it imports.
  examplesTyped <- runIO (mapM (\f -> (,) f <$> loadTyped ("examples/" ++ f)) examples)
  let checked src = either (const Nothing) Just (parseFile "test" src >>= \u -> typed False (prelude ++ [u]) Nothing)
      -- The types of a source's definitions, by name.
      types src = case checked src of
        Nothing -> []
        Just (Typed hir found _) ->
          [ (nameText info, prettyScheme hir scheme)
            | b <- programBindings hir,
              fmap originFile (origin hir (bindingId b)) == Just "test",
              x <- bound b,
              Just info <- [nameInfo hir x],
              Just scheme <- [schemeOf found x]
          ]
      typeOfName src x = lookup x (types src)
      -- What is wrong with a source's types, each with where it is.
      wrong src = case checked src of
        Nothing -> [("does not resolve", Nothing)]
        Just (Typed _ _ failure) -> [(problemMessage p, fst <$> problemSpan p) | p <- failureProblems failure]
      ok src = null (wrong src)
      -- What is wrong with the types of several modules taken together.
      messagesIn files = either (const ["does not resolve"]) (\(Typed _ _ failure) -> map problemMessage (failureProblems failure)) (mapM (uncurry parseFile) files >>= \us -> typed False (prelude ++ us) Nothing)
      messages = map fst . wrong
      tests =
        [ -- Inference.
          ("a number", typeOfName "x = 1" "x" == Just "Integer"),
          ("a character and a string", types "c = 'a'\ns = \"ab\"" == [("c", "Char"), ("s", "[Char]")]),
          ("a function of a number", typeOfName "f x = x + 1" "f" == Just "Num a => a -> a"),
          ("the identity is polymorphic", typeOfName "i x = x" "i" == Just "a -> a"),
          ("and is used at two types", typeOfName "i x = x\np = (i 1, i 'a')" "p" == Just "(Integer, Char)"),
          ("composition", typeOfName "c f g x = f (g x)" "c" == Just "(a -> b) -> (c -> a) -> c -> b"),
          ("a lambda", typeOfName "k = \\x y -> x" "k" == Just "a -> b -> a"),
          ("a list", typeOfName "xs = [1, 2, 3]" "xs" == Just "[Integer]"),
          ("a tuple and the unit", typeOfName "t = (1, 'a', ())" "t" == Just "(Integer, Char, ())"),
          ("recursion over a list", typeOfName "len [] = 0\nlen (_ : xs) = 1 + len xs" "len" == Just "Num b => [a] -> b"),
          ("mutual recursion", types "ev n = if n == 0 then True else od (n - 1)\nod n = if n == 0 then False else ev (n - 1)" == [("ev", "(Eq a, Num a) => a -> Bool"), ("od", "(Eq a, Num a) => a -> Bool")]),
          ("definitions in any order", typeOfName "a = b\nb = 'x'" "a" == Just "Char"),
          ("a data type's constructors", types "data T a = L | N (T a) a (T a)\nleaf = L\none x = N L x L" == [("leaf", "T a"), ("one", "a -> T a")]),
          ("matching on a data type", typeOfName "data T a = L | N (T a) a (T a)\nsize L = 0\nsize (N l _ r) = size l + 1 + size r" "size" == Just "Num b => T a -> b"),
          ("case and guards", typeOfName "f m | True = case m of { Just x -> x; Nothing -> 0 }" "f" == Just "Num a => Maybe a -> a"),
          ("if", typeOfName "f c = if c then 'a' else 'b'" "f" == Just "Bool -> Char"),
          ("where is in scope and generalised", typeOfName "f x = (i x, i 'a') where i y = y" "f" == Just "a -> (a, Char)"),
          ("let is generalised", typeOfName "p = let i x = x in (i 1, i 'a')" "p" == Just "(Integer, Char)"),
          ("a lambda's argument is not", messages "p = (\\i -> (i True, i 'a')) (\\x -> x)" == ["expected Bool, but this has type Char"]),
          ("a pattern binding is not generalised", typeOfName "(a, b) = (1, 'c')" "b" == Just "Char"),
          ("a local that uses an outer variable keeps it fixed", typeOfName "f x = let g y = x in (g 1, g 'a')" "f" == Just "a -> (a, a)"),
          ("do, in IO", typeOfName "m = do { c <- getChar; putChar c; return c }" "m" == Just "IO Char"),
          ("sections and operators", typeOfName "f = map (+ 1)" "f" == Just "[Integer] -> [Integer]"),
          ("as-patterns", typeOfName "f xs@(x : _) = (x, xs)" "f" == Just "[a] -> (a, [a])"),
          -- Checking against a signature.
          ("a signature is taken as given", typeOfName "i :: Integer -> Integer\ni x = x" "i" == Just "Integer -> Integer"),
          ("a polymorphic signature", ok "i :: a -> a\ni x = x" && typeOfName "i :: a -> a\ni x = x" "i" == Just "a -> a"),
          ("its variables are its own names", typeOfName "pair :: left -> right -> (left, right)\npair x y = (x, y)" "pair" == Just "left -> right -> (left, right)"),
          ("a signature too general is refused", messages "i :: a -> a\ni x = 'c'" == ["expected a, but this has type Char"]),
          ("two of its variables are not the same", messages "f :: a -> b -> a\nf x y = y" == ["expected a, but this has type b"]),
          ("the signature's type reaches a lambda", wrong "f :: Integer -> Char\nf = \\x -> x" == [("expected Char, but this has type Integer", Just (Pos 2 11))]),
          ("and the branches of an if", wrong "f :: Bool -> Char\nf c = if c then 'a' else c" == [("expected Char, but this has type Bool", Just (Pos 2 26))]),
          ("and the alternatives of a case", wrong "f :: Maybe Char -> Char\nf m = case m of { Just c -> c; Nothing -> m }" == [("expected Char, but this has type Maybe Char", Just (Pos 2 43))]),
          ("polymorphic recursion needs, and gets, a signature", ok "data N a = Z | S (N (a, a))\ndepth :: N a -> Integer\ndepth Z = 0\ndepth (S n) = 1 + depth n"),
          ("a signature lets a use come before its definition's type is known", ok "f = g 1\ng :: a -> a\ng x = x"),
          ("a local signature", ok "f x = g x where { g :: a -> a; g y = y }"),
          ("a local signature cannot name the outer type", messages "f x = g where { g :: a; g = x }" == ["this definition is less polymorphic than its signature says: a type variable of the signature has to be the type of something outside it"]),
          ("synonyms are seen through", ok "f :: String -> Int\nf s = length s" && typeOfName "type P a = (a, a)\np :: P Char\np = ('a', 'b')" "p" == Just "(Char, Char)"),
          -- Classes.
          ("a context is what a definition may use", ok "f :: Eq a => a -> a -> Bool\nf x y = x == y" && typeOfName "f :: Eq a => a -> a -> Bool\nf x y = x == y" "f" == Just "Eq a => a -> a -> Bool"),
          ("and what it uses without one is missed", messages "f :: a -> a -> Bool\nf x y = x == y" == ["there is no instance of Eq for a"]),
          ("a class brings its superclasses", ok "f :: Ord a => a -> a -> Bool\nf x y = x == y"),
          ("a context is inferred", typeOfName "f x y = x == y" "f" == Just "Eq a => a -> a -> Bool"),
          ("and has each class once", typeOfName "f x y z = x == y && y == z" "f" == Just "Eq a => a -> a -> a -> Bool"),
          ("a type with no instance", messages "x = 'a' + 'b'" == ["there is no instance of Num for Char"]),
          ("and is missed once, however many things want it", messages "x = 1 + \"foo\"" == ["there is no instance of Num for [Char]"]),
          ("an instance for a list asks one for its elements", messages "x = [not] == [not]" == ["there is no instance of Eq for Bool -> Bool"]),
          ("a class and an instance of it", types "class C a where { op :: a -> Int }\ninstance C Bool where { op b = if b then 1 else 0 }\nx = op True" == [("x", "Int")]),
          ("a method has the class in its type", either (const False) (const True) (parseFile "test" "class C a where { op :: a -> Int }\nf x = op x + 1" >>= \u -> typed False (prelude ++ [u]) Nothing) && typeOfName "class C a where { op :: a -> Int }\nf x = op x + 1" "f" == Just "C a => a -> Int"),
          ("an instance method is checked against the class", messages "class C a where { op :: a -> Int }\ninstance C Bool where { op b = b }" == ["expected Int, but this has type Bool"]),
          ("an instance may use only its context", messages "class C a where { op :: a -> Int }\ninstance C [a] where { op xs = op (head xs) }" == ["there is no instance of C for a"]),
          ("and has what its context gives", ok "class C a where { op :: a -> Int }\ninstance C a => C [a] where { op xs = op (head xs) }"),
          ("an instance must have its superclasses", messages "class Eq a => C a where { op :: a -> Int }\ndata T = T\ninstance C T where { op _ = 1 }" == ["there is no instance of Eq for T"]),
          ("a default method is checked", messages "class C a where { op :: a -> Int; op _ = 'c' }" == ["expected Int, but this has type Char"]),
          ("a class of type constructors", typeOfName "f m = m >>= return" "f" == Just "Monad a => a b -> a b"),
          ("derived instances", ok "data T a = A | B a Int deriving (Eq, Ord, Show)\nx = (A == B 'c' 1, A < B True 2, show (B () 3))"),
          ("a derived instance asks the same of the parameters", messages "data T a = A | B a deriving (Eq)\nx = B not == A" == ["there is no instance of Eq for Bool -> Bool"]),
          ("the monomorphism restriction", types "plus = (+)\nadd x y = x + y" == [("plus", "Integer -> Integer -> Integer"), ("add", "Num a => a -> a -> a")]),
          ("what is restricted is settled by its use", typeOfName "n = 1\nm = n + length []" "n" == Just "Int"),
          ("a number of no particular type is an Integer", typeOfName "s = show (2 + 3)" "s" == Just "[Char]" && ok "s = show (2 + 3)"),
          ("what else is ambiguous is refused", not (ok "s = show (toEnum 1)")),
          ("a number matched needs equality", typeOfName "f 0 = True\nf _ = False" "f" == Just "(Eq a, Num a) => a -> Bool"),
          ("main is an action", typeOfName "main = return ()" "main" == Just "IO ()"),
          -- Kinds.
          ("a type given too many arguments", take 1 (messages "f :: Maybe Int Int\nf = Nothing") == ["this type has kind *, so it cannot be applied to a type"]),
          ("a type given too few", messages "f :: Maybe -> Int\nf _ = 1" == ["this type has kind * -> *, but one of kind * is needed here"]),
          ("a parameter's kind is found from its use", ok "data App f a = App (f a)\nx :: App Maybe Int\nx = App (Just 1)"),
          ("and holds for every use", messages "data App f a = App (f a)\nx :: App Int Int\nx = undefined" == ["this type has kind *, but it is given to a type that takes one of kind * -> *"]),
          ("a kind nothing settles is *", messages "data P a = P\nx :: P Maybe\nx = P" == ["this type has kind * -> *, but it is given to a type that takes one of kind *"]),
          ("a class of type constructors has their kind", take 1 (messages "instance Functor Int where { fmap _ x = x }") == ["the type of this instance is not of the kind the class's instances are, which is * -> *"]),
          ("types that use each other are settled together", ok "data Rose f a = Rose a (f (Forest f a))\ndata Forest f a = Forest [Rose f a]\nx :: Rose [] Int\nx = Rose 1 []"),
          ("a synonym has the kind of what it stands for", ok "type L = []\nx :: L Int\nx = [1]"),
          -- Modules.
          ("an instance is in scope where imports lead to its module", null (messagesIn [("A", "module A where\nclass C a where { op :: a -> Int }\ndata T = T"), ("B", "module B where\nimport A\ninstance C T where { op _ = 1 }"), ("D", "module D where\nimport A\nimport B\nx = op T")])),
          ("and not where they do not", messagesIn [("A", "module A where\nclass C a where { op :: a -> Int }\ndata T = T"), ("B", "module B where\nimport A\ninstance C T where { op _ = 1 }"), ("D", "module D where\nimport A\nx = op T")] == ["there is no instance of C for T"]),
          ("a module's numbers are settled before another uses them", messagesIn [("A", "module A where\nn = 1"), ("B", "module B where\nimport A\nm = n + length []")] == ["expected Integer, but this has type Int"]),
          ("modules that import each other are settled together", null (messagesIn [("A", "module A where\nimport B\nn = 1\nk = m"), ("B", "module B where\nimport A\nm = n + length []")])),
          ("a newtype is checked as a type of its own", messagesIn [("A", "module A where\nnewtype Age = Age Int\nx :: Int\nx = Age 3")] == ["expected Int, but this has type Age"]),
          ("a C function has the type it is declared with", typeOfName "foreign import ccall \"math.h sin\" c_sin :: Double -> Double\ny = c_sin 1" "y" == Just "Double" && messages "foreign import ccall \"math.h sin\" c_sin :: Double -> Double\ny = c_sin 'c'" == ["expected Double, but this has type Char"]),
          ("an Int goes round at its ends, as a type", typeOfName "x = maxBound + 1 == (minBound :: Int)" "x" == Just "Bool"),
          -- What is wrong.
          ("a mismatch is reported where it is", wrong "f = not 'a'" == [("expected Bool, but this has type Char", Just (Pos 1 9))]),
          ("an argument of the wrong type", messages "f = not ()" == ["expected Bool, but this has type ()"]),
          ("too many arguments", length (wrong "f = not True False") == 1),
          ("a condition that is not a Bool", messages "f = if 'c' then 2 else 3" == ["expected Bool, but this has type Char"]),
          ("branches of different types", messages "f c = if c then () else 'a'" == ["expected (), but this has type Char"]),
          ("a list of different types", messages "xs = [True, 'a']" == ["expected [Bool], but this has type [Char]"]),
          ("a type that would contain itself", length (wrong "f x = x x") == 1),
          ("a constructor pattern with the wrong number of fields is caught", not (ok "f (Just x y) = x")),
          ("a pattern of the wrong type", messages "f :: Integer -> Integer\nf (Just x) = x" == ["expected Integer, but this has type Maybe _"]),
          ("every mistake is reported, not only the first", length (wrong "a = not 'x'\nb = not ()\nc = [True, 'c']") == 3),
          ("and what is right still has its type", typeOfName "a = not 'x'\ngood x = x" "good" == Just "a -> a"),
          ("an equation with the wrong number of arguments for its signature", not (ok "f :: Integer\nf x = x")),
          -- The table.
          ("every expression and pattern has a type", maybe False covered (checked "f (x : xs) y = if y then [x] else xs\ng = \\a -> case a of { Just b -> (b, 'c'); Nothing -> (1, 'd') }")),
          ("a node's type is its own, not its definition's", maybe False argumentIsChar (checked "f = 'a'\ng = (1, f)")),
          ("no unknowns are left in a definition's type", all (\(_, t) -> '_' `notElem` t) (types "a x = x\nb = a\nc f = f 1")),
          -- The prelude and the examples.
          ("the prelude is well typed", ok "main = return ()"),
          ("the prelude's map", typeOfPrelude prelude "map" == Just "(a -> b) -> [a] -> [b]"),
          ("the prelude's foldr", typeOfPrelude prelude "foldr" == Just "(a -> b -> b) -> b -> [a] -> b"),
          ("the prelude's >>=", typeOfPrelude prelude "error" == Just "[Char] -> a"),
          ("the prelude's zip and unzip", (typeOfPrelude prelude "zip", typeOfPrelude prelude "unzip") == (Just "[a] -> [b] -> [(a, b)]", Just "[(a, b)] -> ([a], [b])")),
          ("the prelude's error", typeOfPrelude prelude "error" == Just "[Char] -> a")
        ]
          ++ [(f ++ " is well typed", either (const False) (\(Typed _ _ failure) -> null (failureProblems failure)) loaded) | (f, loaded) <- examplesTyped]
  forM_ tests (\(name, holds) -> it name (holds `shouldBe` True))
  where
    -- Every node that is an expression or a pattern is in the table.
    covered (Typed hir found _) = all (isJust . typeOf found) [n | n <- nodeIds hir, IntMap.member (nodeKey n) (programOrigins hir), isOwn hir n]
    isOwn hir n = fmap originFile (origin hir n) == Just "test"
    -- In g = (1, f), the node for f has type Char.
    argumentIsChar (Typed hir found _) =
      "Char" `elem` [prettyType hir t | n <- nodeIds hir, isOwn hir n, fmap originSpan (origin hir n) == Just (Pos 2 9, Pos 2 10), Just t <- [typeOf found n]]
    typeOfPrelude prelude x = case typed False prelude Nothing of
      Right (Typed hir found _) ->
        case [prettyScheme hir s | b <- programBindings hir, n <- bound b, fmap nameText (nameInfo hir n) == Just x, Just s <- [schemeOf found n]] of
          t : _ -> Just t
          [] -> Nothing
      Left _ -> Nothing
