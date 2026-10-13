-- | The shared front end: source text to a Core program.
module Powder.Frontend
  ( Unit (..),
    unitModule,
    loadUnits,
    loadBase,
    imported,
    Module,
    Expr,
    Failure,
    readPrelude,
    parseFile,
    parseFileWith,
    Settings (..),
    defaultSettings,
    haskell2010,
    loadUnitsWith,
    loadTypedWith,
    loadProgramWith,
    loadHirWith,
    importedWith,
    parseLineWith,
    Input (..),
    parseLine,
    Asked (..),
    resolved,
    Typed (..),
    typed,
    loadTyped,
    compiled,
    rooted,
    check,
    program,
    loadProgram,
    loadHir,
    sequent,
    axcut,
  )
where

import Control.Monad
import Data.Bifunctor (first)
import Paths_powder (getDataFileName)
import Powder.Core
import Powder.Diagnostics
import qualified Powder.HIR as HIR
import Powder.Kinds (kindcheck)
import Powder.Lower
import qualified Powder.AxCut as AxCut
import qualified Powder.AxCut.Lower as AxCut
import qualified Powder.Sequent as Sequent
import Powder.Sequent.FromCore (fromCore)
import Powder.Simplify (shaken)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Powder.Cpp (preprocess)
import Powder.Extensions (Extension (CPP, ReportPrelude), Extensions, Settings (..), defaultSettings, extensionsOf, haskell2010, pragmaWords, reportPrelude)
import Powder.Lexer (lexer)
import Powder.Parser
import Powder.Syntax.Kind (SyntaxKind (Pragma), isTrivia)
import qualified Data.Text as Text
import Powder.Resolve
import Powder.Typecheck (Checked, typecheck)
import Powder.Syntax.AST (Expr, Module, cast, moduleDecls, moduleHeader)
import qualified Powder.Syntax.AST.Decl as Decl
import qualified Powder.Syntax.Red as Red
import Powder.Syntax.Token (Pos (..), Token (..))
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, (<.>), (</>))

-- | A piece of syntax, with the file it is from and that file's text, for
-- pointing into when something is wrong with it.
--
-- A piece that is a module's may be made part of another module than the
-- one its header names: what is typed at a prompt is part of the module
-- the prompt is in.
data Unit a = Unit {unitPath :: FilePath, unitSource :: String, unitSyntax :: a, unitJoins :: Maybe Name, unitExtensions :: Extensions}

-- | The module a piece of source is, or is part of. With no header it is
-- @Main@.
unitModule :: Unit Module -> Name
unitModule u = fromMaybe (maybe "Main" fst (moduleHeader (unitSyntax u))) (unitJoins u)

-- | The prelude's path and contents.
readPrelude :: IO (FilePath, String)
readPrelude = do
  installed <- getDataFileName "lib/Prelude.hs"
  exists <- doesFileExist installed
  let path = if exists then installed else "lib/Prelude.hs"
  (,) path <$> readFile path

-- | The tree a parse gave, if nothing was wrong with the input.
parsed :: FilePath -> String -> Parsed -> Either Failure Red.Red
parsed path src result = case parsedProblems result of
  [] -> Right (Red.root (parsedTree result))
  problems -> Left (Failure [(path, src)] [p {problemFile = path} | p <- problems])

-- | Parse a source file.
parseFile :: FilePath -> String -> Either Failure (Unit Module)
parseFile = parseFileWith defaultSettings

-- | Parse a source file, given the edition and extensions asked for where
-- it is compiled. What the file's own pragmas say comes after that.
parseFileWith :: Settings -> FilePath -> String -> Either Failure (Unit Module)
parseFileWith settings path src = do
  extensions <- first (\wrong -> Failure [(path, src)] [Problem "error" message path Nothing False | message <- wrong]) (extensionsOf settings (pragmasOf src))
  -- In a literate file, program text marked line by line has a blank line
  -- between it and anything else.
  when (".lhs" `isSuffixOf` path) . forM_ (crowded src) $ \line ->
    Left (Failure [(path, src)] [Problem "syntax error" "program text in a literate file must have a blank line between it and what is not program text" path (Just (Pos line 1, Pos line 2)) False])
  -- A literate file is its program text, the rest left blank, so that
  -- everything in it is where it was.
  -- With CPP the preprocessor's directives are carried out first. What
  -- they leave out is left blank, as for a literate file.
  text <-
    if CPP `Set.member` extensions
      then first (\(line, message) -> Failure [(path, src)] [Problem "syntax error" message path (Just (Pos line 1, Pos line 2)) False]) (preprocess src)
      else pure src
  tree <- parsed path src (parseModuleWith extensions (if ".lhs" `isSuffixOf` path then unlit text else text))
  maybe (Left (Failure [] [Problem "error" (path ++ " is not a module") "" Nothing False])) (Right . (\m -> Unit path src m Nothing extensions)) (cast tree)

-- | The lines of a literate source that are program text hard against
-- text that is not.
crowded :: String -> [Int]
crowded src = [n | (n, before, l, after) <- zip4 [1 ..] ("" : ls) ls (drop 1 ls ++ [""]), marked l, any other [before, after]]
  where
    ls = lines src
    marked l = take 1 l == ">"
    other l = not (marked l) && any (`notElem` " \t") l
    zip4 (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4 as bs cs ds
    zip4 _ _ _ _ = []

-- | The program in a literate source: the lines marked with a @>@, and
-- those between @\\begin{code}@ and @\\end{code}@.
unlit :: String -> String
unlit = unlines . go False . lines
  where
    go _ [] = []
    go code (l : ls)
      | "\\begin{code}" `isPrefixOf` l = "" : go True ls
      | "\\end{code}" `isPrefixOf` l = "" : go False ls
      | code = l : go True ls
      | '>' : rest <- l = (' ' : rest) : go False ls
      | otherwise = "" : go False ls

-- | The words of the pragmas at a file's head that say what language it
-- is in.
pragmasOf :: String -> [String]
pragmasOf src = concat [pragmaWords (Text.unpack (tokenText t)) | t <- takeWhile (\t -> isTrivia (tokenKind t) || tokenKind t == Pragma) (lexer src), tokenKind t == Pragma]

-- | What is typed at a prompt.
data Input = Declarations (Unit Module) | Expression (Unit Expr)

-- | Parse what is typed at a prompt, under a name to report it by.
parseLine :: FilePath -> String -> Either Failure Input
parseLine = parseLineWith defaultSettings

parseLineWith :: Settings -> FilePath -> String -> Either Failure Input
parseLineWith settings path src = do
  extensions <- first (\wrong -> Failure [(path, src)] [Problem "error" message path Nothing False | message <- wrong]) (extensionsOf settings [])
  tree <- parsed path src (parseInputWith extensions src)
  case (cast tree, [e | n <- Red.nodes tree, Just e <- [cast n]]) of
    (Just m, _) -> Right (Declarations (Unit path src m Nothing extensions))
    (Nothing, e : _) -> Right (Expression (Unit path src e Nothing extensions))
    _ -> Left (Failure [] [Problem "error" "nothing to evaluate" "" Nothing False])

-- | An expression to go with some modules: what a prompt was asked. It
-- is bound to a name, and if its value is to be printed, the action that
-- prints it is bound to another.
data Asked = Asked {askedName :: Name, askedPrinter :: Maybe Name, askedExpr :: Unit Expr}

-- | Resolve the names of modules taken together, into HIR. Later modules
-- override earlier ones.
resolved :: [Unit Module] -> Maybe Asked -> Either Failure HIR.Program
resolved units extra =
  first (Failure (sourcesOf units extra)) $
    -- What is typed at a prompt may define again what its module imports.
    resolve (Map.fromList [(unitPath u, unitExtensions u) | u <- units]) (mapMaybe unitJoins units) [(unitPath u, unitModule u, unitSyntax u) | u <- units] ((\(Asked name printer u) -> (name, printer, unitPath u, unitSyntax u)) <$> extra)

sourcesOf :: [Unit Module] -> Maybe Asked -> [(FilePath, String)]
sourcesOf units extra = [(unitPath u, unitSource u) | u <- units] ++ [(unitPath u, unitSource u) | Just (Asked _ _ u) <- [extra]]

-- | A program with its types: the HIR, the table of types beside it, and
-- what the type checker found wrong, which does not stop there being
-- types.
data Typed = Typed {typedProgram :: HIR.Program, typedTypes :: Checked, typedProblems :: Failure}

-- | Resolve modules and check their types. An expression given a name
-- may be given the most general type it has, or else the type it has
-- when it is evaluated, where the monomorphism restriction holds and a
-- number of no particular type is an integer.
typed :: Bool -> [Unit Module] -> Maybe Asked -> Either Failure Typed
typed general units extra = do
  hir <- resolved units extra
  let asked = [f | general, Just (Asked name _ _) <- [extra], HIR.Binding _ (HIR.Function f _) <- HIR.programBindings hir, fmap HIR.nameText (HIR.nameInfo hir f) == Just name]
      (found, problems) = typecheck asked hir
  -- Kinds first: a type put together wrongly is why its uses go wrong.
  pure (Typed hir found (Failure (sourcesOf units extra) (kindcheck hir ++ problems)))

-- | Load a program together with the prelude, and check its types.
loadTyped :: FilePath -> IO (Either Failure Typed)
loadTyped = loadTypedWith defaultSettings

loadTypedWith :: Settings -> FilePath -> IO (Either Failure Typed)
loadTypedWith settings path = fmap (>>= (\units -> typed False units Nothing)) (loadUnitsWith settings path)

-- | Resolve modules, check their types and lower them to Core.
check :: [Unit Module] -> Maybe Asked -> Either Failure Program
check units extra = typed False units extra >>= compiled

-- | A program whose types have been checked, lowered to Core. There is
-- none if its types are wrong.
compiled :: Typed -> Either Failure Program
compiled (Typed hir found problems) = do
  unless (null (failureProblems problems)) (Left problems)
  first (\p -> problems {failureProblems = [p]}) (lower hir found)

-- | The program that computes one name, keeping only what it reaches.
program :: Name -> Maybe Asked -> [Unit Module] -> Either Failure Program
program root asked units = check units asked >>= rooted root . begun
  where
    -- A program's main is run by what the library has to run one, which
    -- sees to what is to be done when it ends.
    begun = if root == "main" then started root "Powder.Base.top" else id

-- | Only what one name reaches.
rooted :: Name -> Program -> Either Failure Program
rooted root prog = do
  unless (root `elem` map fst (progDefs prog)) $
    Left (Failure [] [Problem "error" ("no definition of " ++ root) "" Nothing False])
  pure (shaken root prog)

-- | Load a program together with the prelude.
loadProgram :: FilePath -> IO (Either Failure Program)
loadProgram = loadProgramWith defaultSettings

loadProgramWith :: Settings -> FilePath -> IO (Either Failure Program)
loadProgramWith settings path = fmap (>>= program "main" Nothing) (loadUnitsWith settings path)

-- | A Core program in the sequent calculus, started by running the action
-- one of its definitions is.
sequent :: Name -> Program -> Sequent.Program
sequent = fromCore

-- | And in AxCut, which a machine runs.
axcut :: Name -> Program -> AxCut.Program
axcut root = AxCut.lower . sequent root

-- | Load a program together with the prelude, as far as HIR.
loadHir :: FilePath -> IO (Either Failure HIR.Program)
loadHir = loadHirWith defaultSettings

loadHirWith :: Settings -> FilePath -> IO (Either Failure HIR.Program)
loadHirWith settings path = fmap (>>= (`resolved` Nothing)) (loadUnitsWith settings path)

-- | The prelude and the modules it is made from. They come before
-- everything else, the prelude last of them.
loadBase :: IO (Either Failure [Unit Module])
loadBase = do
  (path, src) <- readPrelude
  -- The libraries are written in the Report's language.
  case parseFileWith haskell2010 path src of
    Left failure -> pure (Left failure)
    Right prelude -> fmap (++ [prelude]) <$> chase [(takeDirectory path, haskell2010)] [unitModule prelude] [] (importsOf prelude)

-- | A program's modules: the prelude's, the file given, and every module
-- that imports lead to from it. A module is looked for by its name, @A.B@
-- in @A/B.hs@, from the file's directory and then among the libraries.
-- They are a package: all of them are compiled together, and they may
-- import each other in a circle. The file given comes last.
loadUnits :: FilePath -> IO (Either Failure [Unit Module])
loadUnits = loadUnitsWith defaultSettings

-- | The same, with the edition and extensions the program's own modules
-- are written in, unless they say otherwise themselves.
loadUnitsWith :: Settings -> FilePath -> IO (Either Failure [Unit Module])
loadUnitsWith settings path = do
  src <- readFile path
  loadBase >>= \case
    Left failure -> pure (Left failure)
    Right base -> case parseFileWith settings path src of
      Left failure -> pure (Left failure)
      Right main' -> fmap (\found -> base ++ found ++ [main']) <$> importedWith settings (takeDirectory path) (unitModule main' : map unitModule base) [main']

-- | The modules some pieces of source import, and those import, that are
-- not among the ones named: looked for from a directory, and then among
-- the libraries.
imported :: FilePath -> [Name] -> [Unit Module] -> IO (Either Failure [Unit Module])
imported = importedWith defaultSettings

importedWith :: Settings -> FilePath -> [Name] -> [Unit Module] -> IO (Either Failure [Unit Module])
importedWith settings dir known units = do
  (prelude, _) <- readPrelude
  chase [(dir, settings), (takeDirectory prelude, haskell2010)] known [] (concatMap importsOf units)

importsOf :: Unit Module -> [Name]
importsOf u =
  [name | Decl.Import name _ _ _ <- moduleDecls (unitSyntax u)]
    -- Its prelude, which no import names.
    ++ [reportPrelude | Set.member ReportPrelude (unitExtensions u)]

-- | Look for modules by name in some directories, and for what they
-- import. A module found nowhere is left for name resolution to miss.
-- Each directory has the settings its modules are parsed with.
chase :: [(FilePath, Settings)] -> [Name] -> [Unit Module] -> [Name] -> IO (Either Failure [Unit Module])
chase _ _ found [] = pure (Right (reverse found))
chase dirs seen found (name : rest)
  | name `elem` seen = chase dirs seen found rest
  | otherwise = do
      let files = [(dir </> map (\c -> if c == '.' then '/' else c) name <.> extension, settings) | (dir, settings) <- dirs, extension <- ["hs", "lhs"]]
      existing <- filterM (doesFileExist . fst) files
      case existing of
        [] -> chase dirs (name : seen) found rest
        (file, settings) : _ -> do
          text <- readFile file
          case parseFileWith settings file text of
            Left failure -> pure (Left failure)
            Right u -> chase dirs (name : seen) (u : found) (rest ++ importsOf u)
