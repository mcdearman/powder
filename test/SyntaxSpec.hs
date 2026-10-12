-- | The lexer, the trees and the typed layer over them.
module SyntaxSpec (spec) where

import Data.List (isSuffixOf, sort)
import qualified Data.Text as Text
import Powder.Diagnostics (Problem (..))
import Powder.Layout (Stream (..), layout)
import Powder.Lexer
import Powder.Parser
import Powder.Syntax.AST (Decl, Expr, cast, moduleDecls, operatorName, syntax)
import qualified Powder.Syntax.AST as Syntax
import qualified Powder.Syntax.AST.Decl as Decl
import qualified Powder.Syntax.AST.Expr as Expr
import qualified Powder.Syntax.AST.Pat as Pat
import qualified Powder.Syntax.AST.Stmt as Stmt
import qualified Powder.Syntax.Green as Green
import Powder.Syntax.Kind
import qualified Powder.Syntax.Red as Red
import Powder.Syntax.Token
import System.Directory (listDirectory)
import Test.Hspec

spec :: Spec
spec = do
  examples <- runIO (map ("examples/" ++) . sort . filter (".hs" `isSuffixOf`) <$> listDirectory "examples")
  sources <- runIO (mapM (\f -> (,) f <$> readFile f) ("lib/Prelude.hs" : examples))
  describe "the lexer" (each lexerTests)
  describe "the tree" (each treeTests)
  describe "the typed layer" (each astTests)
  describe "whole files" (each (concatMap fileTests sources))
  where
    each = mapM_ (\(name, ok) -> it name (ok `shouldBe` True))

type Test = (String, Bool)

-- | The kinds and texts of the tokens that mean something.
toks :: String -> [(SyntaxKind, String)]
toks s = [(tokenKind t, Text.unpack (tokenText t)) | t <- lexer s, not (isTrivia (tokenKind t))]

kinds :: String -> [SyntaxKind]
kinds = map fst . toks

-- | Every kind, trivia too.
allKinds :: String -> [SyntaxKind]
allKinds = map tokenKind . lexer

spelled :: String -> String
spelled = concatMap (Text.unpack . tokenText) . lexer

lexerTests :: [Test]
lexerTests =
  [ ("names", toks "x x' _y Foo Foo_1 a1" == [(VarId, "x"), (VarId, "x'"), (VarId, "_y"), (ConId, "Foo"), (ConId, "Foo_1"), (VarId, "a1")]),
    ("reserved words", kinds "case class data default deriving do else foreign if import in infix infixl infixr instance let module newtype of then type where _" == [KwCase, KwClass, KwData, KwDefault, KwDeriving, KwDo, KwElse, KwForeign, KwIf, KwImport, KwIn, KwInfix, KwInfixl, KwInfixr, KwInstance, KwLet, KwModule, KwNewtype, KwOf, KwThen, KwType, KwWhere, Underscore]),
    ("a reserved word inside a name is a name", kinds "cases ofx letter do'" == [VarId, VarId, VarId, VarId]),
    ("reserved operators", kinds ".. : :: = \\ | <- -> @ =>" == [DotDot, Colon, DoubleColon, Equals, Backslash, Bar, LeftArrow, RightArrow, At, DoubleArrow]),
    -- An operator is told by what touches it, as GHC tells it.
    ("a prefix ~ or ! is a mark", kinds "f ~x !y" == [VarId, Tilde, VarId, Bang, VarId] && kinds "~(a, b) ![c]" == [Tilde, LParen, VarId, Comma, VarId, RParen, Bang, LBracket, VarId, RBracket]),
    ("loose, tight or suffix, it is an operator", all ((== [VarId, VarSym, VarId]) . kinds) ["a ~ b", "a ! b", "a~b", "a!b", "a~ b", "a! b"]),
    ("as it is in parentheses, or before a comment", kinds "(!) (~) !{- c -} x" == [LParen, VarSym, RParen, LParen, VarSym, RParen, VarSym, VarId]),
    ("other operators are operators however they sit", all ((== [VarId, VarSym, VarId]) . kinds) ["a + b", "a+b", "a +b", "a+ b", "a -b", "a .b"]),
    ("operators", toks "+ >>= <$> . ! :+ :| ==> ..." == [(VarSym, "+"), (VarSym, ">>="), (VarSym, "<$>"), (VarSym, "."), (VarSym, "!"), (ConSym, ":+"), (ConSym, ":|"), (VarSym, "==>"), (VarSym, "...")]),
    ("a reserved operator inside an operator is an operator", kinds "|| == <-> =>>" == [VarSym, VarSym, VarSym, VarSym]),
    ("punctuation", kinds "( ) , ; [ ] ` { }" == [LParen, RParen, Comma, Semicolon, LBracket, RBracket, Backquote, LBrace, RBrace]),
    ("qualified names", toks "M.x M.N.y M.T M.+ M.:+ M.." == [(QVarId, "M.x"), (QVarId, "M.N.y"), (QConId, "M.T"), (QVarSym, "M.+"), (QConSym, "M.:+"), (QVarSym, "M..")]),
    ("composition with spaces is not qualification", kinds "f . g" == [VarId, VarSym, VarId]),
    ("integers", toks "0 42 0x1F 0XaB 0o17 0O7" == map (IntegerLit,) ["0", "42", "0x1F", "0XaB", "0o17", "0O7"]),
    ("floats", toks "1.5 2e10 3.0E-2 4.5e+1" == map (FloatLit,) ["1.5", "2e10", "3.0E-2", "4.5e+1"]),
    ("a range is not a float", kinds "[1..3]" == [LBracket, IntegerLit, DotDot, IntegerLit, RBracket]),
    ("a number then a name", toks "1e 0x" == [(IntegerLit, "1"), (VarId, "e"), (IntegerLit, "0"), (VarId, "x")]),
    ("characters", toks "'a' '\\n' '\\'' '\\\\' '\\x41' '\\NUL' '\\^A' '\"'" == map (CharLit,) ["'a'", "'\\n'", "'\\''", "'\\\\'", "'\\x41'", "'\\NUL'", "'\\^A'", "'\"'"]),
    ("a prime in a name is not a character", kinds "f' x' 'a'" == [VarId, VarId, CharLit]),
    ("strings", toks "\"\" \"a b\" \"q\\\"q\" \"\\\\\" \"a\\SOH\\&9\"" == map (StringLit,) ["\"\"", "\"a b\"", "\"q\\\"q\"", "\"\\\\\"", "\"a\\SOH\\&9\""]),
    ("a string gap runs over lines", toks "\"ab\\\n   \\cd\" x" == [(StringLit, "\"ab\\\n   \\cd\""), (VarId, "x")]),
    ("a string that does not end is an error, to the end of the line", toks "\"abc\nx" == [(Error, "\"abc"), (VarId, "x")]),
    ("line comments", allKinds "x -- note\ny" == [VarId, Whitespace, LineComment, Whitespace, VarId]),
    ("dashes alone are a comment", allKinds "---\n" == [LineComment, Whitespace] && allKinds "--" == [LineComment]),
    ("dashes then a symbol are an operator", toks "--> --| |--" == [(VarSym, "-->"), (VarSym, "--|"), (VarSym, "|--")]),
    ("but more dashes then text are a comment", allKinds "--- x" == [LineComment]),
    ("block comments nest", allKinds "a {- b {- c -} d -} e" == [VarId, Whitespace, BlockComment, Whitespace, VarId]),
    ("a block comment holds line breaks", allKinds "{- a\n b -}" == [BlockComment]),
    ("a block comment that does not end is an error", allKinds "x {- y" == [VarId, Whitespace, Error]),
    ("pragmas", allKinds "{-# LANGUAGE LambdaCase #-}" == [Pragma]),
    ("letters beyond ASCII", toks "α Ωmega x₁ naïve 日本" == [(VarId, "α"), (ConId, "Ωmega"), (VarId, "x₁"), (VarId, "naïve"), (VarId, "日本")]),
    ("symbols beyond ASCII", toks "a ∘ b → c" == [(VarId, "a"), (VarSym, "∘"), (VarId, "b"), (VarSym, "→"), (VarId, "c")]),
    ("a stray character is an error of its own", toks "x \1 y" == [(VarId, "x"), (Error, "\1"), (VarId, "y")]),
    ("a lone quote is an error", kinds "' x" == [Error, VarId]),
    ("tabs, form feeds and odd spaces are white space", allKinds "a\t\f\160b" == [VarId, Whitespace, VarId]),
    ("nothing is lost", all (\s -> spelled s == s) awkward)
  ]

-- | Text the lexer has to get through whole, sensible or not.
awkward :: [String]
awkward =
  [ "",
    "\n\n",
    "x = \"unterminated",
    "{- never closed",
    "'",
    "''",
    "\\",
    "a\0b\1c",
    "f x = x -- trailing",
    "  \t  ",
    "{-# OPTIONS #-}\nmodule M where\n",
    "x = 'a' : \"b\\\n  \\c\" ++ [1.5e3, 0xFF]"
  ]

-- | A source, parsed as a module.
tree :: String -> Maybe Red.Red
tree s = case parseModule s of
  Parsed g [] -> Just (Red.root g)
  _ -> Nothing

-- | The tree of a source, right or wrong.
anyTree :: String -> Red.Red
anyTree = Red.root . parsedTree . parseModule

problemsOf :: String -> [Problem]
problemsOf = parsedProblems . parseModule

-- | The same source, with every implicit block written out.
braced :: String -> String
braced s = concatMap spell (Red.descendants (anyTree s))
  where
    spell t
      | not (Red.isToken t) = ""
      | otherwise = case Red.kind t of
          VLBrace -> "{"
          VRBrace -> "}"
          VSemicolon -> ";"
          _ -> Text.unpack (Red.text t)

sample :: String
sample = "-- a\nf x =\n  case x of\n    Just y -> y + 1 -- inc\n    Nothing -> 0\n"

treeTests :: [Test]
treeTests =
  [ ("a tree spells its source", fmap (Text.unpack . Red.text) (tree sample) == Just sample),
    ("comments and white space are in it", maybe False (any ((== LineComment) . Red.kind) . Red.descendants) (tree sample)),
    ("layout's tokens are in it, with no text", maybe False (\r -> [Red.width t | t <- Red.descendants r, isVirtual (Red.kind t)] == [0, 0, 0, 0, 0]) (tree sample)),
    ("the root is a module at offset 0", fmap (\r -> (Red.kind r, Red.offset r, Red.width r)) (tree sample) == Just (Module, 0, length sample)),
    ("children start where the one before ends", maybe False contiguous (tree sample)),
    ("a child knows its parent", maybe False (\r -> all (\c -> fmap Red.offset (Red.parent c) == Just (Red.offset r)) (Red.children r)) (tree sample)),
    ("the token at an offset", fmap (fmap (Text.unpack . Red.text) . Red.tokenAt 27) (tree sample) == Just (Just "Just")),
    ("no token past the end", fmap (fmap Red.kind . Red.tokenAt 999) (tree sample) == Just Nothing),
    ("a token's line and column", (tree sample >>= Red.tokenAt 27 >>= Just . Red.position) == Just (Pos 4 5)),
    ("ancestors lead to the root", (tree sample >>= Red.tokenAt 27 >>= Just . map Red.kind . Red.ancestors) == Just [ConId, ConPat, Alt, CaseExpr, Rhs, FunClause, Module]),
    ("siblings", (tree "x = 1\ny = 2" >>= firstOf FunClause >>= Red.nextSibling >>= Just . Red.kind) == Just VSemicolon),
    ("equal syntax is an equal green tree", fmap (map Green.kind . take 1) (greens "f = 1\nf = 1") == Just [FunClause] && fmap same (greens "f = 1\nf = 1") == Just True),
    ("layout adds only tokens with no text", let (ls, _, end) = lexemes (lexer sample) in concatMap (Text.unpack . tokenText) (flatten (layout ls end)) == concatMap (Text.unpack . tokenText . lexemeToken) ls),
    -- Layout: what indentation alone decides.
    ("a block's items are separated", braced "f = 1\ng = 2" == "{f = 1;\ng = 2}"),
    ("a deeper line carries on the item", braced "f = 1 +\n  2\ng = 3" == "{f = 1 +\n  2;\ng = 3}"),
    ("a shallower line closes blocks", braced "f = x\n  where\n    x = 1\ng = 2" == "{f = x\n  where{\n    x = 1};\ng = 2}"),
    ("the end closes whatever is open", braced "f = do\n  a\n  b" == "{f = do{\n  a;\n  b}}"),
    ("a block with nothing in it", braced "f = x where\ng = 1" == "{f = x where{};\ng = 1}"),
    ("explicit braces are left alone", braced "f = do { a; b }" == "{f = do { a; b }}"),
    -- Layout: what only the parser can decide.
    ("in closes a let block", braced "f = let x = 1 in x" == "{f = let{ x = 1} in x}" && null (problemsOf "f = let x = 1 in x")),
    ("in closes a let block, and a case in it", null (problemsOf "f = let x = case y of A -> 1 in x")),
    ("a bracket closes the blocks inside it", braced "f = (case x of A -> 1)" == "{f = (case x of{ A -> 1})}" && null (problemsOf "f = (case x of A -> 1)")),
    ("so does a comma", null (problemsOf "f = [do a, b]") && null (problemsOf "f = (do a, do b)")),
    ("then and else close a do block", null (problemsOf "f = if c then do a else do b")),
    ("of closes a do block", null (problemsOf "f = case do a of X -> 1")),
    ("an explicit brace closes an implicit block inside it", braced "f = do { a; case x of A -> b }" == "{f = do { a; case x of{ A -> b} }}" && null (problemsOf "f = do { a; case x of A -> b }")),
    ("a where level with a block's items ends the block", null (problemsOf "f x = case x of\n  A -> y\n  B -> z\n  where\n  y = 1\n  z = 2")),
    ("nested blocks close one after another", braced "f = (case a of X -> do b)" == "{f = (case a of{ X -> do{ b}})}"),
    -- Errors.
    ("a tree with errors still spells its source", all (\s -> Text.unpack (Red.text (anyTree s)) == s) broken),
    ("and says what is wrong", not (any (null . problemsOf) broken)),
    ("what cannot be used is in an error node", any ((== ErrorNode) . Red.kind) (Red.descendants (anyTree "f = 1 )\ng = 2"))),
    ("an error does not spoil what follows", [Text.unpack (Red.text n) | n <- Red.descendants (anyTree "f = 1 )\ng = 2"), Red.kind n == FunClause] == ["f = 1", "\ng = 2"]),
    ("one mistake is one problem", map length [problemsOf "f = 1 ) ) )\ng = 2", problemsOf "f = (1 + \ng = 2", problemsOf "data = A"] == [1, 1, 1]),
    ("a problem says where it is", [problemSpan p | p <- problemsOf "f = 1\ng = )"] == [Just (Pos 2 5, Pos 2 6)]),
    ("a problem at the end says so", map problemAtEnd (problemsOf "f x =") == [True]),
    ("a problem elsewhere does not", map problemAtEnd (problemsOf "f x = )") == [False]),
    ("an empty do block is a problem", not (null (problemsOf "f = do"))),
    ("what is no token is said to be so", map problemMessage (concatMap problemsOf ["x = \"abc", "x = 1 {- c", "x = \1"]) == ["this string does not end", "this comment does not end", "this character is not part of any token"])
  ]
  where
    contiguous r = and (zipWith (\a b -> Red.end a == Red.offset b) (Red.children r) (drop 1 (Red.children r))) && all contiguous (Red.children r)
    firstOf k r = case [n | n <- Red.descendants r, Red.kind n == k] of n : _ -> Just n; [] -> Nothing
    greens s = (\r -> [Red.green n | n <- Red.descendants r, Red.kind n == FunClause]) <$> tree s
    -- The second clause has a line break before it that the first has not.
    same gs = case gs of [a, b] -> Green.children a /= [] && map Green.kind (Green.children a) == map Green.kind (Green.children b); _ -> False
    flatten = \case
      Next l rest _ -> lexemeToken l : flatten rest
      End _ -> []
    broken = ["f = )", "f = (1 +", "= 3", "f x = case x of", "data T = | A", "f = let in", "x = \"abc", "f = [1, 2", "f = \\ -> 1", "f = if a then b", "{- open"]

-- | The declarations of a source.
declsOf :: String -> [Decl]
declsOf s = maybe [] moduleDecls (tree s >>= cast)

-- | The expression a source defines @x@ as.
exprOf :: String -> Maybe Expr
exprOf s = case declsOf ("x = " ++ s) of
  [Decl.Clause _ _ (Syntax.Rhs [([], e)] _)] -> Just e
  _ -> Nothing

-- | A sketch of an expression, to compare against.
sketch :: Expr -> String
sketch = \case
  Expr.Var x -> x
  Expr.Con c -> c
  Expr.Lit l -> show l
  Expr.App f args -> "(" ++ unwords (map sketch (f : args)) ++ ")"
  Expr.Infix items -> "<" ++ unwords (map item items) ++ ">"
  Expr.Lam ps e -> "\\" ++ show (length ps) ++ "." ++ sketch e
  Expr.Let ds e -> "let" ++ show (length ds) ++ "." ++ sketch e
  Expr.If c t e -> "if(" ++ sketch c ++ "," ++ sketch t ++ "," ++ sketch e ++ ")"
  Expr.Case e alts -> "case(" ++ sketch e ++ ";" ++ unwords [pat p | Syntax.Alt p _ <- alts] ++ ")"
  Expr.Do stmts -> "do(" ++ unwords (map stmt stmts) ++ ")"
  Expr.Paren e -> "(" ++ sketch e ++ ")"
  Expr.Tuple es -> "tuple" ++ show (map sketch es)
  Expr.List es -> "list" ++ show (map sketch es)
  Expr.Range from next to -> "range(" ++ sketch from ++ maybe "" (("," ++) . sketch) next ++ maybe "" ((".." ++) . sketch) to ++ ")"
  Expr.Comprehension e qs -> "[" ++ sketch e ++ " | " ++ show (length qs) ++ "]"
  Expr.SectionL e o -> "(" ++ sketch e ++ " " ++ operatorName o ++ "_)"
  Expr.SectionR o e -> "(_" ++ operatorName o ++ " " ++ sketch e ++ ")"
  Expr.Typed e _ -> "(" ++ sketch e ++ " :: _)"
  Expr.Record e fs -> sketch e ++ "{" ++ show (length fs) ++ "}"
  where
    item = \case
      Expr.Operand e -> sketch e
      Expr.Operator o -> operatorName o
      Expr.Negate -> "neg"
    stmt = \case
      Stmt.Bind p e -> pat p ++ "<-" ++ sketch e
      Stmt.Let ds -> "let" ++ show (length ds)
      Stmt.Expr e -> sketch e

pat :: Pat.Pat -> String
pat = \case
  Pat.Var x -> x
  Pat.Wild -> "_"
  Pat.Lit l -> show l
  Pat.NegLit l -> "-" ++ show l
  Pat.Con c ps -> unwords (c : map pat ps)
  Pat.Infix a c b -> pat a ++ c ++ pat b
  Pat.Lazy p -> "~" ++ pat p
  Pat.Record c fs -> c ++ "{" ++ show (length fs) ++ "}"
  Pat.As x p -> x ++ "@" ++ pat p
  Pat.Paren p -> "(" ++ pat p ++ ")"
  Pat.Tuple ps -> "tuple" ++ show (map pat ps)
  Pat.List ps -> "list" ++ show (map pat ps)

astTests :: [Test]
astTests =
  [ ("application", fmap sketch (exprOf "f a b") == Just "(f a b)"),
    ("operators stay in a row", fmap sketch (exprOf "a + b * c") == Just "<a + b * c>"),
    ("a prefix minus", fmap sketch (exprOf "- a `div` 2") == Just "<neg a div Number 2>"),
    ("operators as names", fmap sketch (exprOf "(+) 1 (:)") == Just "((+) Number 1 :)" || fmap sketch (exprOf "(+) 1 (:)") == Just "(+ Number 1 :)"),
    ("sections", fmap sketch (exprOf "(+ 1) (2 *)") == Just "((_+ Number 1) (Number 2 *_))"),
    ("a negative number in brackets is not a section", fmap sketch (exprOf "(- 1)") == Just "(<neg Number 1>)"),
    ("lambda, let and if", fmap sketch (exprOf "\\a b -> let c = a in if c then b else c") == Just "\\2.let1.if(c,b,c)"),
    ("case", fmap sketch (exprOf "case m of { Just (a, _) -> a; xs@(y : _) -> y; [] -> 0; -1 -> 1 }") == Just "case(m;Just tuple[\"a\",\"_\"] xs@(y:_) list[] -Number 1)"),
    ("do", fmap sketch (exprOf "do { a <- f; let { b = a }; g b }") == Just "do(a<-f let1 (g b))"),
    ("tuples, lists and ranges", fmap sketch (exprOf "((), (a, b), [a, b], [a ..], [a .. b])") == Just "tuple[\"tuple[]\",\"tuple[\\\"a\\\",\\\"b\\\"]\",\"list[\\\"a\\\",\\\"b\\\"]\",\"range(a)\",\"range(a..b)\"]"),
    ("literals", fmap sketch (exprOf "f 0x10 'a' \"b\\n\" 1.5") == Just "(f Number 16 Character 'a' Text \"b\\n\" Fraction \"1.5\")"),
    ("a literal that means nothing", fmap sketch (exprOf "'\\q'") == Just "Malformed \"'\\\\q'\""),
    ("declarations", map declSketch (declsOf "import M\ntype T = Int\ndata D a = A | B a Int\ninfixr 5 +++, `op`\nf, g :: Int\nf 0 = 1\nx +++ y = x\n(a, b) = p") == ["import M", "type", "data D [A/0,B/2]", "infixr 5 +++ op", "sig f g", "clause f/1", "clause +++/2", "bind tuple[\"a\",\"b\"]"]),
    ("a class and an instance", map declSketch (declsOf "class Eq a => C a where\n  op :: a -> a\n  op x = x\ninstance C Int where\n  op = id\nclass D a") == ["class/2", "instance/1", "class/0"]),
    ("deriving", map snd (concatMap Decl.derived (declsOf "data T = A | B deriving (Eq, Show)\ndata U = U deriving Ord\ndata V = V")) == ["Eq", "Show", "Ord"]),
    ("a list comprehension", fmap sketch (exprOf "[x | x <- xs, let y = x, p y]") == Just "[x | 3]"),
    ("ranges with a step", (fmap sketch (exprOf "[a, b ..]"), fmap sketch (exprOf "[a, b .. c]")) == (Just "range(a,b)", Just "range(a,b..c)")),
    ("a guard of several qualifiers", [map (length . fst) alts | Decl.Clause _ _ (Syntax.Rhs alts _) <- declsOf "f x | Just y <- x, y = 1 | otherwise = 2"] == [[2, 1]]),
    ("a lazy pattern and a strict field", map declSketch (declsOf "f ~(a, b) = a\ndata T = T !Int ! Int") == ["clause f/1", "data T [T/2]"]),
    ("an expression said to have a type", fmap sketch (exprOf "x :: Int") == Just "(x :: _)"),
    ("a foreign import and a default", map declSketch (declsOf "foreign import ccall unsafe \"math.h sin\" c_sin :: Double -> Double\nforeign import ccall getpid :: IO Int\ndefault (Int, Double)") == ["foreign Just \"math.h sin\" c_sin", "foreign Nothing getpid", "default/2"]),
    ("guards and where", [(length alts, length ds) | Decl.Clause _ _ (Syntax.Rhs alts ds) <- declsOf "f x\n  | x = 1\n  | otherwise = 2\n  where\n    a = 1\n    b = 2"] == [(2, 2)]),
    ("a fixity with no precedence is 9", [p | Decl.Fixity _ p _ <- declsOf "infixl +"] == [9]),
    ("fixities are found at any depth", fmap (length . Syntax.fixityDecls) (tree "f = x where { infixl 6 +; x = 1 }" >>= cast) == Just 1),
    ("a typed node gives back its syntax", fmap (Text.unpack . Red.text . syntax) (exprOf "f  {- c -} a") == Just " f  {- c -} a"),
    ("a node is only cast to what it is", (tree "x = 1" >>= (cast :: Red.Red -> Maybe Expr)) `seq'` True)
  ]
  where
    declSketch = \case
      Decl.Data t _ cs -> "data " ++ t ++ " [" ++ commas [c ++ "/" ++ show (length fields) | Syntax.Constructor c fields <- cs] ++ "]"
      Decl.Clause f ps _ -> "clause " ++ f ++ "/" ++ show (length ps)
      Decl.PatBind p _ -> "bind " ++ pat p
      Decl.Signature names _ -> unwords ("sig" : names)
      Decl.Fixity assoc p os -> unwords ((case assoc of Decl.LeftAssoc -> "infixl"; Decl.RightAssoc -> "infixr"; Decl.NonAssoc -> "infix") : show p : os)
      Decl.TypeSynonym {} -> "type"
      Decl.Class _ ds -> "class/" ++ show (length ds)
      Decl.Instance _ ds -> "instance/" ++ show (length ds)
      Decl.Default ts -> "default/" ++ show (length ts)
      Decl.Foreign entity x _ -> unwords ["foreign", show entity, x]
      Decl.Import name qualified alias items -> unwords (["import", name] ++ ["qualified" | qualified] ++ maybe [] (\a -> ["as", a]) alias ++ maybe [] (\(hiding, listed) -> [(if hiding then "hiding " else "") ++ show (length listed)]) items)
    commas = foldr1 (\a b -> a ++ "," ++ b)
    seq' m b = maybe b (const False) m

-- | For a real file: the lexer loses nothing, and neither does the parser.
fileTests :: (FilePath, String) -> [Test]
fileTests (path, src) =
  [ (path ++ ": tokens spell the file", spelled src == src),
    (path ++ ": no lexical errors", Error `notElem` allKinds src),
    (path ++ ": the tree spells the file", fmap (Text.unpack . Red.text) (tree src) == Just src)
  ]
