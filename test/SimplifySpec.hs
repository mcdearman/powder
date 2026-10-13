-- | Making a program smaller: what is rewritten, and what is left alone
-- because rewriting it would do something twice.
module SimplifySpec (spec) where

import Powder.Core
import Powder.Simplify (simplify)
import Test.Hspec

-- | A program of some definitions, made smaller, as it would be shown.
smaller :: [(Name, Core)] -> [String]
smaller defs = map prettyDef (progDefs (simplify (Program [] defs [])))

-- | What one definition becomes, among others.
becomes :: Name -> [(Name, Core)] -> Maybe String
becomes x defs = lookup x [(y, prettyDef (y, e)) | (y, e) <- progDefs (simplify (Program [] defs []))]

dict :: ConInfo
dict = ConInfo "Dict#C" 0 2 1

-- | The function that is a class's method: one field of its dictionary.
method :: Int -> Core
method i = Lam "$dict" (Case (Var "$dict") [Alt dict ["$a", "$b"] (Var (["$a", "$b"] !! i))])

apply :: Core -> [Core] -> Core
apply = foldl App

spec :: Spec
spec = do
  it "a method of a dictionary that is written out is the definition it finds" $ do
    let defs =
          [ ("first", method 0),
            ("inst#C#T", apply (Con dict) [Lam "x" (Lam "y" (apply (Prim IntAdd) [Var "y", Var "y"])), Lit 7]),
            ("use", Lam "p" (apply (Var "first") [Var "inst#C#T", Var "p", App (Var "opaque") (Var "p")])),
            ("opaque", Lam "x" (App (Var "opaque") (Var "x")))
          ]
    becomes "use" defs `shouldBe` Just "use = \\p -> inst#C#T@0 p (opaque p)"
    becomes "inst#C#T" defs `shouldBe` Just "inst#C#T = Dict#C inst#C#T@0 7"

  it "a coercion applied to something is the thing" $
    becomes "f" [("f", Lam "x" (App (Prim Coerce) (App (Prim Coerce) (Var "x"))))] `shouldBe` Just "f = \\x -> x"

  it "another name for something is that thing" $ do
    let defs = [("same", Prim IntAdd), ("f", Lam "x" (Let (NonRec "y" (Var "x")) (apply (Var "same") [Var "y", Var "y"])))]
    becomes "f" defs `shouldBe` Just "f = \\x -> primIntAdd x x"

  it "a small function is written out where it is called" $ do
    let defs = [("inc", Lam "n" (apply (Prim IntAdd) [Var "n", Lit 1])), ("f", Lam "x" (App (Var "inc") (App (Var "g") (Var "x")))), ("g", Lam "x" (App (Var "g") (Var "x")))]
    becomes "f" defs `shouldBe` Just "f = \\x -> primIntAdd (g x) 1"

  it "but not where that would do something twice" $ do
    let defs = [("double", Lam "n" (apply (Prim IntAdd) [Var "n", Var "n"])), ("f", Lam "x" (App (Var "double") (App (Var "g") (Var "x")))), ("g", Lam "x" (Lam "y" (Var "x")))]
    becomes "f" defs `shouldBe` Just "f = \\x -> double (g x)"

  it "though a name may be had twice" $ do
    let defs = [("double", Lam "n" (apply (Prim IntAdd) [Var "n", Var "n"])), ("f", Lam "x" (App (Var "double") (Var "x")))]
    becomes "f" defs `shouldBe` Just "f = \\x -> primIntAdd x x"

  it "a function that binds a name is left to be called" $ do
    let defs = [("twice", Lam "n" (Let (NonRec "m" (App (Var "h") (Var "n"))) (apply (Prim IntAdd) [Var "m", Var "m"]))), ("f", Lam "x" (App (Var "twice") (Var "x"))), ("h", Lam "x" (Lam "y" (Var "x")))]
    becomes "f" defs `shouldBe` Just "f = \\x -> twice x"

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

  it "keeps every definition it was given" $
    length (smaller [("a", Lit 1), ("b", Var "a"), ("c", Lam "x" (Var "b"))]) `shouldBe` 3
