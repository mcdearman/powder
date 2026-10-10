module Main (main) where

import Control.Exception
import Control.Monad
import Powder.Core
import Powder.Diagnostics (Failure (..), printFailure)
import Powder.Frontend
import qualified Powder.HIR as HIR
import qualified Data.ByteString.Lazy as L
import qualified Powder.AxCut as AxCut
import Powder.Stubs (stubs)
import qualified Powder.Bytecode as Bytecode
import qualified Powder.AxCut.Machine as AxCut
import qualified Powder.Sequent as Sequent
import Powder.SKI.Comb
import Powder.Typecheck (schemeOf)
import Powder.Types (prettyScheme)
import Powder.SKI.Compile
import Powder.SKI.Eval
import Repl
import System.Environment
import System.Exit
import System.IO

data Options = Options
  { interactive :: Bool,
    typecheckOnly :: Bool,
    dumpHir :: Bool,
    dumpCore :: Bool,
    dumpComb :: Bool,
    -- | Run on the machine for AxCut, not on the combinator reducer.
    viaAxCut :: Bool,
    dumpSequent :: Bool,
    dumpAxCut :: Bool,
    -- | Write the program's bytecode image here, and stop.
    emit :: Maybe FilePath,
    -- | Write C for the functions the program exports here, and stop.
    stubsTo :: Maybe FilePath,
    stats :: Bool,
    file :: Maybe FilePath,
    -- | What comes after the file is the program's.
    programArgs :: [String]
  }

usage :: String
usage =
  unlines
    [ "usage: powder-ski [options] [FILE [ARGS]]",
      "",
      "Runs FILE, or starts a REPL if there is none.",
      "",
      "  --repl       start a REPL with FILE loaded",
      "  --typecheck  check the types and print those of the definitions",
      "  --dump-hir   print the program after name resolution and stop",
      "  --dump-core  print the desugared program and stop",
      "  --dump-comb  print the combinator program and stop",
      "  --axcut      run on the machine for AxCut, the sequent calculus backend's",
      "  --dump-sequent  print the program in the sequent calculus and stop",
      "  --dump-axcut    print the program in AxCut and stop",
      "  --emit OUT      write the program's bytecode image, for powder-vm to run",
      "  --stubs OUT     write C for the functions the program exports, for C to link with",
      "  --stats      report program size and reduction count"
    ]

parseArgs :: Options -> [String] -> Either String Options
parseArgs opts = \case
  [] -> Right opts
  "--repl" : rest -> parseArgs opts {interactive = True} rest
  "--typecheck" : rest -> parseArgs opts {typecheckOnly = True} rest
  "--dump-hir" : rest -> parseArgs opts {dumpHir = True} rest
  "--dump-core" : rest -> parseArgs opts {dumpCore = True} rest
  "--dump-comb" : rest -> parseArgs opts {dumpComb = True} rest
  "--axcut" : rest -> parseArgs opts {viaAxCut = True} rest
  "--dump-sequent" : rest -> parseArgs opts {dumpSequent = True} rest
  "--dump-axcut" : rest -> parseArgs opts {dumpAxCut = True} rest
  "--emit" : out : rest -> parseArgs opts {emit = Just out} rest
  "--stubs" : out : rest -> parseArgs opts {stubsTo = Just out} rest
  "--stats" : rest -> parseArgs opts {stats = True} rest
  path : rest | take 1 path /= "-", Nothing <- file opts -> Right opts {file = Just path, programArgs = rest}
  arg : _ -> Left ("unrecognised argument: " ++ arg)

main :: IO ()
main = do
  args <- getArgs
  opts <- either (\msg -> die (msg ++ "\n\n" ++ usage)) pure (parseArgs (Options False False False False False False False False Nothing Nothing False Nothing []) args)
  case file opts of
    Just path | not (interactive opts) -> runFile opts path
    path | typecheckOnly opts || dumpHir opts || dumpCore opts || dumpComb opts || stats opts -> die (maybe usage (const "--repl cannot be combined with other options") path)
    path -> repl path

runFile :: Options -> FilePath -> IO ()
runFile opts path | typecheckOnly opts = do
  Typed hir found problems <- loadTyped path >>= either (\failure -> printFailure stderr failure >> exitFailure) pure
  -- The file's own definitions, without the prelude's.
  let own b = fmap HIR.originFile (HIR.origin hir (HIR.bindingId b)) == Just path
  forM_ (filter own (HIR.programBindings hir)) $ \b -> forM_ (HIR.bound b) $ \x ->
    forM_ ((,) <$> HIR.nameInfo hir x <*> schemeOf found x) $ \(info, scheme) ->
      putStrLn (HIR.nameText info ++ " :: " ++ prettyScheme hir scheme)
  unless (null (failureProblems problems)) (hFlush stdout >> printFailure stderr problems >> exitFailure)
runFile opts path | dumpHir opts = do
  hir <- loadHir path >>= either (\failure -> printFailure stderr failure >> exitFailure) pure
  -- The file's own definitions, without the prelude's.
  let own b = fmap HIR.originFile (HIR.origin hir (HIR.bindingId b)) == Just path
  putStr (HIR.pretty hir {HIR.programBindings = filter own (HIR.programBindings hir)})
runFile opts path = do
  prog <- loadProgram path >>= either (\failure -> printFailure stderr failure >> exitFailure) pure
  let combs = compileProgram prog
  case () of
    _ | dumpCore opts -> mapM_ (putStrLn . prettyDef) (progDefs prog)
    _ | dumpComb opts -> forM_ combs $ \(x, e) -> putStrLn (x ++ " = " ++ pretty e)
    _ | dumpSequent opts -> putStr (Sequent.pretty (sequent "main" prog))
    _ | dumpAxCut opts -> putStr (AxCut.pretty (axcut "main" prog))
    _ | Just out <- stubsTo opts -> writeFile out (stubs (progExports prog))
    _ | Just out <- emit opts -> L.writeFile out (Bytecode.image (axcut "main" prog))
    _ | viaAxCut opts -> do
      n <- AxCut.run stdHooks {programName = path, arguments = programArgs opts} (axcut "main" prog) `catch` \(RuntimeError msg) -> hFlush stdout >> die ("runtime error: " ++ msg)
      when (stats opts) (hPutStrLn stderr (show n ++ " statements"))
    _ -> do
      n <- run stdHooks {programName = path, arguments = programArgs opts, exported = progExports prog} combs `catch` \(RuntimeError msg) -> hFlush stdout >> die ("runtime error: " ++ msg)
      when (stats opts) $
        hPutStrLn stderr $
          show (length combs) ++ " definitions, " ++ show (sum (map (size . snd) combs)) ++ " combinators, " ++ show n ++ " reductions"
