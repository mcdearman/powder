-- | An interactive session.
module Repl (repl) where

import Control.Exception
import Control.Monad (forM_, void, when)
import Data.Char (isAlpha, isAlphaNum, isLower, isSpace, isUpper)
import Data.List (dropWhileEnd, isPrefixOf, isSuffixOf, nub, sort)
import Data.Maybe (fromMaybe, listToMaybe)
import Entry
import Lambline
import Powder.Core
import Powder.Diagnostics (Failure (..), Problem (..), printFailure)
import Powder.Frontend
import qualified Powder.HIR as HIR
import Powder.SKI.Comb (pretty)
import Powder.SKI.Compile
import Powder.SKI.Eval
import Powder.Typecheck (Checked, schemeOf)
import Powder.Types (Scheme (..), prettyScheme)
import qualified Powder.Types as Types
import Powder.Syntax.AST (moduleDecls)
import qualified Powder.Syntax.AST as Syntax
import qualified Powder.Syntax.AST.Decl as Decl
import qualified Powder.Syntax.AST.Expr as Expr
import System.Environment (lookupEnv)
import System.Exit
import System.IO

data Session = Session
  { -- | The prelude, after the modules it is made from.
    prelude :: [Unit Module],
    -- | The file that was loaded, last, after the modules it imports.
    loaded :: [Unit Module],
    -- | The library modules that entries at the prompt have imported.
    libraries :: [Unit Module],
    -- | What has been defined at the prompt, oldest first.
    defined :: [Unit Module]
  }

-- | What is defined at the prompt is part of the module that was loaded,
-- or of @Main@, and a later part's definition of a name replaces an
-- earlier one, for its existing callers too.
modules :: Session -> [Unit Module]
modules s = prelude s ++ libraries s ++ loaded s ++ [blank | null (loaded s)] ++ defined s
  where
    -- So that the prompt is in Main before anything is defined there.
    blank = either (error "Repl: an empty module does not parse") (\m -> m {unitJoins = Just "Main"}) (parseFile "<prompt>" "")

-- | The module the prompt is in.
home :: Session -> Name
home s = case reverse (loaded s) of
  m : _ -> unitModule m
  [] -> "Main"

-- | What an entry is called in a report of what is wrong with it.
entryName :: Session -> FilePath
entryName s = "<entry " ++ show (length (defined s) + 1) ++ ">"

-- | Say what is wrong.
complain :: Failure -> IO ()
complain failure = hFlush stdout >> printFailure stderr failure

help :: String
help =
  unlines
    [ "  EXPR            evaluate: an IO action is run, any other value is shown",
      "  DECLS           define functions, types, classes or instances",
      "  :load FILE      load a file, forgetting what was defined at the prompt",
      "  :reload         load it again",
      "  :comb EXPR      show the combinators an expression or name compiles to",
      "  :type EXPR      show the type of an expression",
      "  :core EXPR      show the Core an expression or name desugars to",
      "  :quit",
      "",
      "Enter submits an entry once it parses. One that does not is carried on,",
      "indented, and a blank line ends it. An entry that opens with a type",
      "signature takes equations until the blank line.",
      "Alt-Enter or Ctrl-J starts a new line whatever the entry looks like.",
      "Up and Down move between the lines of an entry, then through the history,",
      "which is kept in .repl_history. Ctrl-R searches it. Tab completes."
    ]

logo :: String
logo =
  unlines
    [ "      ____                    __",
      "     / __ \\____ _      ______/ /__  _____",
      "    / /_/ / __ \\ | /| / / __  / _ \\/ ___/",
      "   / ____/ /_/ / |/ |/ / /_/ /  __/ /",
      "  /_/    \\____/|__/|__/\\__,_/\\___/_/"
    ]

-- | Whether to colour what is printed: stdout is a terminal, and @NO_COLOR@
-- is not set.
coloured :: IO Bool
coloured = (&&) <$> hIsTerminalDevice stdout <*> (maybe True null <$> lookupEnv "NO_COLOR")

-- | Short, so that the name is still on the screen when the prompt is:
-- everything else there is to say is @:help@'s.
printBanner :: IO ()
printBanner = do
  colour <- coloured
  let paint code text = if colour then "\ESC[" ++ code ++ "m" ++ text ++ "\ESC[0m" else text
      keyName = paint "1;36"
  putStr . unlines $
    [""]
      ++ map (paint "1;32") (lines logo)
      ++ [ "",
           "  " ++ paint "1" "Welcome to the Powder interactive environment!",
           "",
           "  " ++ keyName ":help" ++ " commands and keys   " ++ keyName ":q" ++ " quit",
           "  " ++ keyName "Tab" ++ " completes   " ++ keyName "Ctrl+R" ++ " searches the history   " ++ keyName "Ctrl+J" ++ " starts a new line",
           ""
         ]

-- | Where the history is kept between sessions: in the directory the REPL
-- is started from, as Meadow's is.
historyFile :: FilePath
historyFile = ".repl_history"

repl :: Maybe FilePath -> IO ()
repl path = do
  p <- loadBase >>= either (\failure -> complain failure >> exitFailure) pure
  s <- maybe pure load path (Session p [] [] [])
  printBanner
  -- Only the first line of an entry carries the prompt. The rest start
  -- under it, behind a prompt of spaces, so that what lines up on screen
  -- lines up for the layout rule. Listing the candidates, rather than
  -- cycling through them, matters when a prefix matches a dozen names.
  editor <- newEditor defaultConfig {continuationPrompt = "  ", completionType = List}
  -- A script fed through a pipe is not something to remember.
  remembering <- hIsTerminalDevice stdin
  when remembering (loadHistory editor historyFile)
  loop editor s
  when remembering (saveHistory editor historyFile)

loop :: Editor -> Session -> IO ()
loop editor s = do
  setHelper editor . Just $
    defaultHelper
      { validate = \entry -> if needsContinuation entry then Incomplete else Valid Nothing,
        complete = completeInput s,
        highlight = matchingBracketHighlighter,
        -- Green for our own prompt, and not for a history search's.
        highlightPrompt = \prompt isDefault -> if isDefault then "\ESC[1;32m" ++ prompt ++ "\ESC[0m" else prompt
      }
  -- Enter carries an unfinished entry on, indented, and otherwise falls
  -- through to the validator, which submits it. Alt-Enter and Ctrl-J break
  -- the line whatever the entry looks like; Shift-Enter too, on a terminal
  -- that tells it apart from Enter.
  let breaking always = Conditional (\_ _ _ ctx -> Insert 1 <$> lineBreak always (contextLine ctx) (contextPos ctx))
  bindSequence editor (KeySeq [key '\r']) (breaking False)
  mapM_ (\k -> bindSequence editor (KeySeq [k]) (breaking True)) [alt '\r', KeyEvent KEnter noMods {modShift = True}, ctrl 'j']
  readline editor "> " >>= \case
    Left Eof -> pure ()
    -- Ctrl-C abandons the entry being typed, or the evaluation under way.
    Left Interrupted -> loop editor s
    Right line
      | all isSpace line -> loop editor s
      | otherwise -> do
          _ <- addHistoryEntry editor line
          catchJust interrupt (step s line) (\() -> Just s <$ putStrLn "Interrupted.") >>= maybe (pure ()) (loop editor)
  where
    interrupt e = if e == UserInterrupt then Just () else Nothing

-- | Complete by what the cursor is on: a command, the file for @:load@, a
-- type, or a name. Among names, case says which kind: a capital starts a
-- constructor and anything else a value.
completeInput :: Session -> String -> Int -> Context -> IO (Int, [Candidate])
completeInput s line p ctx
  | any (`isPrefixOf` before) [":l ", ":load "] = filenameCompleter line p ctx
  | ":" `isPrefixOf` before && not (any isSpace before) = pure (0, matching (map (':' :) commands) before)
  | otherwise = pure . (,) start . flip matching word $ case position before of
      Naming -> []
      Type -> types
      Term
        | any isUpper (take 1 word) -> constructors
        | otherwise -> values
  where
    before = take p line
    start = wordStart line p
    word = drop start before
    matching candidates prefix = map candidate (sort (nub (filter (prefix `isPrefixOf`) candidates)))
    commands = ["comb", "core", "help", "load", "quit", "reload", "type"]
    decls = concatMap (moduleDecls . unitSyntax) (modules s)
    values = [name | Decl.Clause name@(c : _) _ _ <- decls, isLower c || c == '_']
    constructors = ["False", "True"] ++ [c | Decl.Data _ _ cs <- decls, Syntax.Constructor c _ <- cs]
    -- Types are read and thrown away, but they are still worth completing.
    types = ["Bool", "Char", "IO", "Int", "Integer", "String"] ++ [t | Decl.Data t _ _ <- decls] ++ [t | Decl.TypeSynonym t _ _ <- decls]

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Handle one input; 'Nothing' ends the session.
step :: Session -> String -> IO (Maybe Session)
step s line = case break isSpace (trim line) of
  ("", _) -> pure (Just s)
  (':' : cmd, arg) -> command cmd (trim arg)
  _ -> Just <$> input s line
  where
    same = pure (Just s)
    command cmd arg
      | cmd `elem` ["q", "quit"] = pure Nothing
      | cmd `elem` ["h", "help", "?"] = putStr help >> same
      | cmd `elem` ["l", "load"], not (null arg) = Just <$> load arg s
      | cmd `elem` ["r", "reload"], m : _ <- reverse (loaded s) = Just <$> load (unitPath m) s
      | cmd `elem` ["r", "reload"] = putStrLn "no file is loaded" >> same
      | cmd `elem` ["t", "type"] = typeOfInput s arg >> same
      | cmd == "comb" = inspect s arg (\prog -> [(x, pretty e) | (x, e) <- compileProgram prog]) >> same
      | cmd == "core" = inspect s arg (\prog -> [(x, drop (length x + 3) (prettyDef d)) | d@(x, _) <- progDefs prog]) >> same
      | otherwise = putStrLn ("unknown command :" ++ cmd ++ "; try :help") >> same

load :: FilePath -> Session -> IO Session
load path s =
  try (loadUnits path) >>= \case
    Left e -> s <$ putStrLn (displayException (e :: IOException))
    Right found -> case found >>= \units -> units <$ check units Nothing of
      Left failure -> s <$ complain failure
      Right units -> s {loaded = drop (length (prelude s)) units, defined = []} <$ putStrLn ("Loaded " ++ path ++ ".")

-- | The name an entered expression is bound to, and the name of the
-- action that prints its value. Neither can clash with a name from the
-- source.
root, printer :: Name
root = "$repl"
printer = "$print"

-- | The program that computes an expression in the session.
bind :: Session -> Unit Expr -> Either Failure Program
bind s e = program root (Just (Asked root Nothing e)) (modules s)

input :: Session -> String -> IO Session
input session line = case parseLine (entryName session) line of
  Left failure -> session <$ complain failure
  -- An entry may import modules, which are then looked for.
  Right (Declarations entered) ->
    imported "." (map unitModule (modules session)) [entered] >>= \case
      Left failure -> session <$ complain failure
      Right found -> accept session {libraries = libraries session ++ found} (Declarations entered)
  Right entered -> accept session entered

-- | Take an entry whose imports are loaded.
accept :: Session -> Input -> IO Session
accept s = \case
  Declarations entered | m <- entered {unitJoins = Just (home s)} -> case typed False (modules s ++ [m]) Nothing of
    Left failure -> s <$ complain failure
    Right checked@(Typed hir found _) -> case compiled checked of
      Left failure -> s <$ complain failure
      Right _ -> do
        -- Say what the entry defined: each name with its type, and each
        -- type and class.
        let own b = fmap HIR.originFile (HIR.origin hir (HIR.bindingId b)) == Just (unitPath m)
        forM_ (filter own (HIR.programBindings hir)) $ \b -> forM_ (HIR.bound b) $ \x ->
          forM_ ((,) <$> HIR.nameInfo hir x <*> schemeOf found x) $ \(info, scheme) ->
            signatureLine (HIR.nameText info) (prettyScheme hir scheme)
        forM_ (moduleDecls (unitSyntax m)) $ \case
          Decl.Data t _ _ -> putStrLn ("defined type " ++ t)
          Decl.TypeSynonym t _ _ -> putStrLn ("defined type " ++ t)
          Decl.Class _ _ -> putStrLn "defined class"
          Decl.Instance _ _ -> putStrLn "defined instance"
          _ -> pure ()
        pure s {defined = defined s ++ [m]}
  -- An expression is first given the most general type it has, which
  -- says whether it is an action. An action is run. Anything else is
  -- printed, at the type it has when it is evaluated, if it can be shown;
  -- and if it cannot, such as a function, its type is all there is to say.
  Expression e -> s <$ case typed True (modules s) (Just (Asked root Nothing e)) of
    Left failure -> complain failure
    Right general@(Typed hir found problems)
      | not (null (failureProblems problems)) -> complain problems
      | isAction hir found -> do
          say e general
          either complain (run' root) (compiled general >>= rooted root)
      | otherwise -> case typed False (modules s) (Just (Asked root (Just printer) e)) of
          Right shown | null (failureProblems (typedProblems shown)) -> do
            say e shown
            either complain (\prog -> putStr "= " >> hFlush stdout >> run' printer prog) (compiled shown >>= rooted printer)
          _ -> say e general
  where
    run' name prog =
      void (eval stdHooks (compileProgram prog) name)
        `catches` [ Handler (\(RuntimeError msg) -> hFlush stdout >> hPutStrLn stderr ("runtime error: " ++ msg)),
                    -- The program called exitWith or error; the session goes on.
                    Handler (\(_ :: ExitCode) -> hFlush stdout)
                  ]
        >> hFlush stdout
    -- An expression that is only a name is asking what that name is.
    say e (Typed hir found _) = do
      let label = case unitSyntax e of
            Expr.Var x -> x
            Expr.Con c -> c
            _ -> "_"
      forM_ (rootScheme hir found) (signatureLine label . prettyScheme hir)
    isAction hir found = case rootScheme hir found of
      Just (Forall _ _ (Types.App (Types.Con c) _)) -> c == HIR.builtinIO (HIR.programBuiltins hir)
      _ -> False

-- | What is wrong with the types of one entry. What is wrong elsewhere is
-- not its business.
complainOf :: Unit a -> Failure -> IO ()
complainOf entry problems = case [p | p <- failureProblems problems, problemFile p == unitPath entry] of
  [] -> pure ()
  wrong -> complain problems {failureProblems = wrong}

-- | The type found for the expression an entry asked about.
rootScheme :: HIR.Program -> Checked -> Maybe Scheme
rootScheme hir found = do
  x <- listToMaybe [x | b <- HIR.programBindings hir, x <- HIR.bound b, fmap HIR.nameText (HIR.nameInfo hir x) == Just root]
  schemeOf found x

-- | @name :: type@, as the prompt says what an entry is: the name bold,
-- and in the type every capitalised word, which is a type's name, in cyan.
signatureLine :: String -> String -> IO ()
signatureLine name scheme = do
  colour <- coloured
  let paint code text = if colour then "\ESC[" ++ code ++ "m" ++ text ++ "\ESC[0m" else text
      -- Operators are shown as they would be written in a signature.
      shown = if any isAlpha (take 1 name) || take 1 name `elem` ["_", "(", "["] then name else "(" ++ name ++ ")"
      painted = \case
        [] -> []
        c : rest
          | isUpper c -> let (word, more) = span (\x -> isAlphaNum x || x `elem` "_'") (c : rest) in paint "36" word ++ painted more
          | isAlpha c || c == '_' -> let (word, more) = span (\x -> isAlphaNum x || x `elem` "_'") (c : rest) in word ++ painted more
          | otherwise -> c : painted rest
  putStrLn (paint "1" shown ++ " :: " ++ painted scheme)

-- | Show the type of an expression, or what is wrong with its types.
typeOfInput :: Session -> String -> IO ()
typeOfInput s arg = case parseLine (entryName s) arg of
  Left failure -> complain failure
  Right (Declarations _) -> putStrLn "expected an expression"
  Right (Expression e) -> case typed True (modules s) (Just (Asked root Nothing e)) of
    Left failure -> complain failure
    Right (Typed hir found problems) -> do
      forM_ (rootScheme hir found) (signatureLine (trim arg) . prettyScheme hir)
      complainOf e problems

-- | Show one definition of the program for an expression: the one it names,
-- if it is just a name, and otherwise the expression itself.
inspect :: Session -> String -> (Program -> [(Name, String)]) -> IO ()
inspect s arg render = case parseLine (entryName s) arg of
  Left failure -> complain failure
  Right (Declarations _) -> putStrLn "expected an expression"
  Right (Expression e) -> case render <$> bind s e of
    Left failure -> complain failure
    Right defs
      -- A name of another module's has that module's before it.
      | Just x <- named (unitSyntax e), d : _ <- [d | (y, d) <- defs, y == x || ("." ++ x) `isSuffixOf` y] -> putStrLn (x ++ " = " ++ d)
      | otherwise -> putStrLn (fromMaybe "" (lookup root defs))
  where
    named = \case Expr.Var x -> Just x; Expr.Con c -> Just c; _ -> Nothing
