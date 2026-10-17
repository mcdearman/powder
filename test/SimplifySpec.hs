-- | Making a program smaller: what is rewritten, and what is left alone
-- because rewriting it would do something twice.
module SimplifySpec (spec) where

import Data.Char (isDigit)
import Powder.Core
import Powder.Simplify (simplify)
import Test.Hspec

-- | A program of some definitions, made smaller, as it would be shown.
smaller :: [(Name, Core)] -> [String]
smaller defs = map prettyDef (progDefs (simplify (Program [] defs [])))

-- | What one definition becomes, among others, without what the names
-- made new have after them.
becomes :: Name -> [(Name, Core)] -> Maybe String
becomes x defs = lookup x [(y, plainly (prettyDef (y, e))) | (y, e) <- progDefs (simplify (Program [] defs []))]

plainly :: String -> String
plainly = \case
  '~' : rest -> plainly (dropWhile isDigit rest)
  c : rest -> c : plainly rest
  [] -> []

pair :: ConInfo
pair = ConInfo "Pair" 0 2 1

dict :: ConInfo
dict = ConInfo "Dict#C" 0 2 1

-- | The function that is a class's method: one field of its dictionary.
method :: Int -> Core
method i = Lam "$dict" (Case (Var "$dict") [Alt dict ["$a", "$b"] (Var (["$a", "$b"] !! i))])

apply :: Core -> [Core] -> Core
apply = foldl App

spec :: Spec
spec = do
  it "a method of a dictionary that is written out is the definition it finds, written out" $ do
    let defs =
          [ ("first", method 0),
            ("inst#C#T", apply (Con dict) [Lam "x" (Lam "y" (apply (Prim IntAdd) [Var "y", Var "y"])), Lit 7]),
            ("use", Lam "p" (apply (Var "first") [Var "inst#C#T", Var "p", App (Var "opaque") (Var "p")])),
            ("opaque", Lam "x" (App (Var "opaque") (Var "x")))
          ]
    becomes "use" defs `shouldBe` Just "use = \\p -> let y = opaque p in primIntAdd y y"
    becomes "inst#C#T" defs `shouldBe` Just "inst#C#T = Dict#C inst#C#T@0 7"

  it "a coercion applied to something is the thing" $
    becomes "f" [("f", Lam "x" (App (Prim Coerce) (App (Prim Coerce) (Var "x"))))] `shouldBe` Just "f = \\x -> x"

  it "another name for something is that thing" $ do
    let defs = [("same", Prim IntAdd), ("f", Lam "x" (Let (NonRec "y" (Var "x")) (apply (Var "same") [Var "y", Var "y"])))]
    becomes "f" defs `shouldBe` Just "f = \\x -> primIntAdd x x"

  it "a small function is written out where it is called" $ do
    let defs = [("inc", Lam "n" (apply (Prim IntAdd) [Var "n", Lit 1])), ("f", Lam "x" (App (Var "inc") (App (Var "g") (Var "x")))), ("g", Lam "x" (App (Var "g") (Var "x")))]
    becomes "f" defs `shouldBe` Just "f = \\x -> primIntAdd (g x) 1"

  it "and what is not a name is bound once, so that it is not worked out twice" $ do
    let defs = [("double", Lam "n" (apply (Prim IntAdd) [Var "n", Var "n"])), ("f", Lam "x" (App (Var "double") (App (Var "g") (Var "x")))), ("g", Lam "x" (Lam "y" (Var "x")))]
    becomes "f" defs `shouldBe` Just "f = \\x -> let n = g x in primIntAdd n n"

  it "though a name may be had twice" $ do
    let defs = [("double", Lam "n" (apply (Prim IntAdd) [Var "n", Var "n"])), ("f", Lam "x" (App (Var "double") (Var "x")))]
    becomes "f" defs `shouldBe` Just "f = \\x -> primIntAdd x x"

  it "a function that binds a name is written out too, its names new" $ do
    let defs = [("twice", Lam "n" (Let (NonRec "m" (App (Var "h") (Var "n"))) (apply (Prim IntAdd) [Var "m", Var "m"]))), ("f", Lam "x" (App (Var "twice") (Var "x"))), ("h", Lam "x" (Lam "y" (Var "x")))]
    becomes "f" defs `shouldBe` Just "f = \\x -> let m = h x in primIntAdd m m"

  it "a function that calls itself is left to be called" $ do
    let defs = [("loop", Lam "n" (App (Var "loop") (Var "n"))), ("f", Lam "x" (App (Var "loop") (Var "x")))]
    becomes "f" defs `shouldBe` Just "f = \\x -> loop x"

  it "an instance that needs another is made once for the one it is given" $ do
    let defs =
          [ ("second", method 1),
            ("inst#C#Int", apply (Con dict) [Lit 1, Lit 2]),
            -- Its second field asks for itself again, as one for lists does.
            ("inst#C#[]", Lam "d" (apply (Con dict) [App (Var "second") (Var "d"), App (Var "second") (App (Var "inst#C#[]") (Var "d"))])),
            ("use", App (Var "second") (App (Var "inst#C#[]") (Var "inst#C#Int")))
          ]
        made = "inst#C#[]<inst#C#Int>"
    becomes made defs `shouldBe` Just (made ++ " = Dict#C 2 " ++ made ++ "@1")
    becomes (made ++ "@1") defs `shouldBe` Just (made ++ "@1 = " ++ made ++ "@1")
    becomes "use" defs `shouldBe` Just ("use = " ++ made ++ "@1")

  it "a function applied is its body, its parameter bound to the argument" $ do
    let defs = [("f", Lam "x" (App (Lam "y" (apply (Prim IntAdd) [Var "y", Var "y"])) (App (Var "g") (Var "x")))), ("g", Lam "x" (App (Var "g") (Var "x")))]
    becomes "f" defs `shouldBe` Just "f = \\x -> let y = g x in primIntAdd y y"

  it "what is bound and not used is gone, and what is used once is put there" $ do
    let defs = [("f", Lam "x" (Let (NonRec "unused" (App (Var "g") (Var "x"))) (Let (NonRec "once" (App (Var "g") (Var "x"))) (apply (Prim IntAdd) [Var "once", Lit 1])))), ("g", Lam "x" (App (Var "g") (Var "x")))]
    becomes "f" defs `shouldBe` Just "f = \\x -> primIntAdd (g x) 1"

  it "what is used once inside a function is left bound, unless it is no work" $ do
    let defs =
          [ ("f", Lam "x" (Let (NonRec "work" (App (Var "g") (Var "x"))) (Let (NonRec "fun" (Lam "z" (Var "z"))) (apply (Con pair) [Lam "y" (Var "work"), Lam "y" (App (Var "fun") (Var "y"))])))),
            ("g", Lam "x" (App (Var "g") (Var "x")))
          ]
    becomes "f" defs `shouldBe` Just "f = \\x -> let work = g x in Pair (\\y -> work) (\\y -> y)"

  it "a case of a constructor it can see is the clause for it" $ do
    let defs =
          [ ("built", Case (apply (Con pair) [Lit 1, Lit 2]) [Alt pair ["a", "b"] (apply (Prim IntSub) [Var "a", Var "b"])]),
            ("bound", Lam "x" (Let (NonRec "p" (apply (Con pair) [Var "x", Lit 2])) (apply (Con pair) [Var "p", Case (Var "p") [Alt pair ["a", "b"] (Var "a")]]))),
            ("inside", Lam "p" (Case (Var "p") [Alt pair ["a", "b"] (Case (Var "p") [Alt pair ["c", "d"] (Var "d")])])),
            ("truth", Case (apply (Prim IntLt) [Lit 1, Lit 2]) [Alt falseCon [] (Lit 0), Alt trueCon [] (Lit 1)])
          ]
    becomes "built" defs `shouldBe` Just "built = -1"
    becomes "bound" defs `shouldBe` Just "bound = \\x -> Pair (Pair x 2) x"
    becomes "inside" defs `shouldBe` Just "inside = \\p -> case p of { Pair a b -> b }"
    becomes "truth" defs `shouldBe` Just "truth = 1"

  it "a case of a case with one clause is the outer one in its clause" $ do
    let defs = [("f", Lam "q" (Case (Case (Var "q") [Alt pair ["a", "b"] (apply (Con pair) [Var "b", Var "a"])]) [Alt pair ["c", "d"] (Var "c")]))]
    becomes "f" defs `shouldBe` Just "f = \\q -> case q of { Pair a b -> b }"

  it "keeps every definition it was given" $
    length (smaller [("a", Lit 1), ("b", Var "a"), ("c", Lam "x" (Var "b"))]) `shouldBe` 3
