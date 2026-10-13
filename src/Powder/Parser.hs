-- | A hand-written recursive-descent parser that builds a green tree.
--
-- It is written the way Alex Kladov's "Resilient LL Parsing Tutorial"
-- lays out: each function of the grammar looks at the tokens ahead to
-- decide what it is at, never tries something and takes it back, and
-- records what it does as a flat list of events, from which the tree is
-- built afterwards. It always produces a tree. Where the input is wrong it
-- says so, wraps what it cannot use in an 'ErrorNode', and carries on.
--
-- Every token it is given ends up in the tree, in order, trivia and the
-- layout pass's additions included, so the tree's text is the input.
--
-- The tokens come from the layout pass, which leaves one question to the
-- parser: whether an implicit block ends where the next token would not
-- fit. 'expect' is the only place that is answered.
module Powder.Parser
  ( Parsed (..),
    parseModule,
    parseModuleWith,
    parseInput,
    parseInputWith,
  )
where

import Control.Monad
import Control.Monad.State.Strict
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as Text
import Powder.Diagnostics (Problem (..))
import Powder.Extensions (Extension (..), Extensions)
import Powder.Layout (Stream (..), layout)
import Powder.Layout qualified as Layout
import Powder.Lexer
import Powder.Syntax.Green (Green)
import Powder.Syntax.Green qualified as Green
import Powder.Syntax.Kind
import Powder.Syntax.Token

-- | A tree, and what was wrong with the input, in the order it was found.
-- The tree is there whatever was wrong.
data Parsed = Parsed {parsedTree :: Green, parsedProblems :: [Problem]}

-- | Parse a source file into a tree whose root is a 'Module'.
parseModule :: String -> Parsed
parseModule = parseModuleWith Set.empty

-- | Parse a source file written with some extensions.
parseModuleWith :: Extensions -> String -> Parsed
parseModuleWith extensions = parseWith extensions sourceFile

-- | Parse what is typed at a prompt: declarations, giving a 'Module', or
-- an expression, giving a 'ReplExpr'.
parseInput :: String -> Parsed
parseInput = parseInputWith Set.empty

parseInputWith :: Extensions -> String -> Parsed
parseInputWith extensions = parseWith extensions $ do
  declares <- startsDeclaration
  if declares then sourceFile else replExpr

parseWith :: Extensions -> P () -> String -> Parsed
parseWith extensions grammar src = Parsed (buildTree (toList (events final)) trailing) (reverse (problems final))
  where
    (ls, trailing, end) = lexemes (lexerWith extensions src)
    final = execState grammar (Parser (layout ls end) 0 256 Seq.empty [] extensions)

-- The parser ----------------------------------------------------------------

data Event
  = -- | Start a node. Its kind is filled in when it is closed.
    Open SyntaxKind
  | Close
  | -- | Take a token into the node being built.
    Advance Lexeme

data Parser = Parser
  { -- | The tokens to come.
    stream :: Stream,
    -- | How many tokens have been taken.
    pos :: !Int,
    -- | Spent by looking ahead and restored by advancing, so that a
    -- function that loops without advancing is caught at once.
    fuel :: !Int,
    events :: Seq Event,
    problems :: [Problem],
    -- | The extensions the source is written with.
    enabled :: Extensions
  }

type P = State Parser

newtype MarkOpened = MarkOpened Int

newtype MarkClosed = MarkClosed Int

-- | Whether an extension is on.
on :: Extension -> P Bool
on extension = gets (Set.member extension . enabled)

-- | Start a node here.
open :: P MarkOpened
open = do
  mark <- gets (Seq.length . events)
  modify (\p -> p {events = events p |> Open ErrorNode})
  pure (MarkOpened mark)

-- | Start a node around one that is already finished: for what turns out,
-- after its first part is read, to be something bigger.
openBefore :: MarkClosed -> P MarkOpened
openBefore (MarkClosed mark) = do
  modify (\p -> p {events = Seq.insertAt mark (Open ErrorNode) (events p)})
  pure (MarkOpened mark)

close :: MarkOpened -> SyntaxKind -> P MarkClosed
close (MarkOpened mark) k = do
  modify (\p -> p {events = Seq.update mark (Open k) (events p) |> Close})
  pure (MarkClosed mark)

close_ :: MarkOpened -> SyntaxKind -> P ()
close_ mark k = void (close mark k)

eof :: P Bool
eof = gets $ \p -> case stream p of
  End _ -> True
  Next {} -> False

advance :: P ()
advance = modify $ \p -> case stream p of
  Next l rest _ -> p {stream = rest, pos = pos p + 1, fuel = 256, events = events p |> Advance l}
  End _ -> p

-- | The kind of the token so many ahead; 'Eof' past the last.
nth :: Int -> P SyntaxKind
nth n = do
  p <- get
  when (fuel p == 0) (error "Powder.Parser: the parser is stuck")
  put p {fuel = fuel p - 1}
  pure (case drop n (Layout.kinds (stream p)) of k : _ -> k; [] -> Eof)

at :: SyntaxKind -> P Bool
at k = (== k) <$> nth 0

atAny :: [SyntaxKind] -> P Bool
atAny ks = (`elem` ks) <$> nth 0

eat :: SyntaxKind -> P Bool
eat k = do
  here <- at k
  when here advance
  pure here

-- | Take the token that has to come next, or say that it is missing.
--
-- This is where the layout rule's last clause is applied: an implicit
-- block ends where the next token would otherwise be an error. If what has
-- to come next is the brace that closes one, and it is not there, the
-- tokens are read again as the layout pass gives them with the block
-- closed here, and then the brace is there.
expect :: SyntaxKind -> P Bool
expect k = do
  here <- at k
  closing <- gets $ \p -> case stream p of
    Next _ _ closed | k == VRBrace -> closed
    _ -> Nothing
  case closing of
    _ | here -> True <$ advance
    Just closed -> True <$ (modify (\p -> p {stream = closed}) >> advance)
    Nothing -> False <$ problem ("expected " ++ describe k)

expect_ :: SyntaxKind -> P ()
expect_ = void . expect

expectAny :: [SyntaxKind] -> String -> P ()
expectAny ks what = do
  here <- atAny ks
  if here then advance else problem ("expected " ++ what)

-- | Note something wrong at the token the parser is at. One thing wrong
-- with a token is enough: whatever else is said of it follows from the
-- first. And if the token is no token at all, that is what is wrong.
problem :: String -> P ()
problem message = modify $ \p ->
  let found = case stream p of
        Next l _ _ ->
          let Pos line col = lexemePos l
              spelling = tokenText (lexemeToken l)
              width = Text.length (Text.takeWhile (/= '\n') spelling)
              said
                | tokenKind (lexemeToken l) /= Error = message
                | Text.pack "\"" `Text.isPrefixOf` spelling = "this string does not end"
                | Text.pack "{-" `Text.isPrefixOf` spelling = "this comment does not end"
                | otherwise = "this character is not part of any token"
           in Problem "syntax error" said "" (Just (lexemePos l, Pos line (col + max 1 width))) (col == 0)
        End end -> Problem "syntax error" message "" (Just (end, end)) True
      repeated = case problems p of
        last' : _ -> fmap fst (problemSpan last') == fmap fst (problemSpan found)
        [] -> False
   in if repeated then p else p {problems = found : problems p}

-- | Take a token that fits nowhere, saying so.
advanceWithError :: String -> P ()
advanceWithError message = do
  m <- open
  problem message
  advance
  close_ m ErrorNode

-- | A node for something that should have been here and was not.
missing :: String -> P MarkClosed
missing message = do
  m <- open
  problem message
  close m ErrorNode

describe :: SyntaxKind -> String
describe = \case
  Equals -> "="
  RightArrow -> "->"
  LeftArrow -> "<-"
  DoubleColon -> "::"
  RParen -> ")"
  RBrace -> "}"
  VRBrace -> "the end of the block"
  RBracket -> "]"
  Backquote -> "`"
  KwIn -> "in"
  KwThen -> "then"
  KwElse -> "else"
  KwOf -> "of"
  KwWhere -> "where"
  ConId -> "a capitalised name"
  IntegerLit -> "a number"
  k -> show k

-- | Turn the events into a tree: the nodes they open and close, with each
-- token, and the trivia before it, in the node that was open when the
-- parser advanced over it. What trivia is left goes at the end of the root.
buildTree :: [Event] -> [Token] -> Green
buildTree evs trailing = go evs []
  where
    leaf t = Green.token (tokenKind t) (tokenText t)
    go :: [Event] -> [(SyntaxKind, [Green])] -> Green
    go (Open k : rest) stack = go rest ((k, []) : stack)
    go (Advance l : rest) ((k, cs) : stack) =
      go rest ((k, leaf (lexemeToken l) : map leaf (reverse (lexemeTrivia l)) ++ cs) : stack)
    go (Close : rest) ((k, cs) : stack) = case stack of
      (k', cs') : above -> go rest ((k', Green.node k (reverse cs) : cs') : above)
      [] -> Green.node k (reverse cs ++ map leaf trailing)
    go _ _ = error "Powder.Parser: the events do not make a tree"

-- Looking further ahead -------------------------------------------------------

-- | The kinds of the tokens from so many ahead, to the end, as they are if
-- no block is closed early.
ahead :: Int -> P [SyntaxKind]
ahead n = gets (\p -> drop n (Layout.kinds (stream p)) ++ [Eof])

semis :: [SyntaxKind]
semis = [Semicolon, VSemicolon]

-- | What a declaration or statement is can depend on a token well into it:
-- @f x = ...@ and @f x :: ...@ start alike. So look along it, outside any
-- brackets and blocks, for the first token that settles the matter, noting
-- on the way whether an infix operator or a @:@ went by.
data Scan = Scan {stoppedAt :: SyntaxKind, sawOperator :: Bool, sawColon :: Bool}

scan :: [SyntaxKind] -> [SyntaxKind] -> Scan
scan deciding = go [] False False
  where
    -- The brackets and blocks open so far, innermost first.
    go :: [SyntaxKind] -> Bool -> Bool -> [SyntaxKind] -> Scan
    go stack op colon = \case
      [] -> Scan Eof op colon
      k : rest
        | k `elem` [LParen, LBracket, LBrace, VLBrace] -> go (k : stack) op colon rest
        -- A bracket closes the implicit blocks opened inside it, which the
        -- tokens ahead do not yet show as closed.
        | k `elem` [RParen, RBracket] -> case dropWhile (== VLBrace) stack of
            _ : outer -> go outer op colon rest
            [] -> Scan Eof op colon
        | k `elem` [RBrace, VRBrace] -> case stack of
            _ : outer -> go outer op colon rest
            [] -> Scan Eof op colon
        | k == KwIn, VLBrace : outer <- stack -> go outer op colon rest
        | not (null stack) -> go stack op colon rest
        | k `elem` deciding || k `elem` Eof : semis -> Scan k op colon
        | k `elem` [VarSym, QVarSym, Backquote] -> go stack True colon rest
        | k == Colon -> go stack op True rest
        | otherwise -> go stack op colon rest

data Shape = Signature | Function | InfixFunction | ParenFunction | PatternBinding

-- | What kind of declaration starts here.
declShape :: P Shape
declShape = do
  kinds <- ahead 0
  let found = scan [DoubleColon, Equals, Bar] kinds
      named = case kinds of
        VarId : At : _ -> False
        VarId : _ -> True
        LParen : op : RParen : _ -> op `elem` [VarSym, QVarSym]
        _ -> False
  pure $ case () of
    _
      | stoppedAt found == DoubleColon -> Signature
      | sawOperator found -> InfixFunction
      | named && not (sawColon found) -> Function
      | parenthesisedHead kinds -> ParenFunction
      | otherwise -> PatternBinding
  where
    -- @(x op y) z ... =@: an operator defined with more arguments than
    -- its two. After the closing parenthesis comes a pattern, not the @=@.
    parenthesisedHead = \case
      LParen : rest -> inside (1 :: Int) False rest
      _ -> False
    inside depth operator' = \case
      [] -> False
      k : rest
        | k `elem` [LParen, LBracket] -> inside (depth + 1) operator' rest
        | k `elem` [RParen, RBracket] -> if depth == 1 then operator' && any (`elem` patStarts) (take 1 rest) else inside (depth - 1) operator' rest
        | depth == 1 && k `elem` [VarSym, Backquote] -> inside depth True rest
        | otherwise -> inside depth operator' rest

-- | Whether what is typed at a prompt is declarations rather than an
-- expression: it opens with a word only a declaration does, or has a @=@,
-- a guard or a @::@ outside any brackets. The input starts with the brace
-- the layout pass opens it with.
startsDeclaration :: P Bool
startsDeclaration = do
  kinds <- ahead 1
  pure $ case kinds of
    k : _ | k `elem` [KwData, KwNewtype, KwType, KwImport, KwInfix, KwInfixl, KwInfixr, KwModule, KwClass, KwInstance] -> True
    [Eof] -> True
    VRBrace : _ -> True
    _ -> case stoppedAt (scan [DoubleColon, Equals, Bar] kinds) of
      -- A signature comes with a definition. With none, it is an
      -- expression said to have a type.
      DoubleColon -> signed kinds && Equals `elem` kinds
      k -> k `elem` [Equals, Bar]
  where
    signed = \case
      VarId : rest -> more rest
      LParen : _ : RParen : rest -> more rest
      _ -> False
    more = \case
      Comma : rest -> signed rest
      DoubleColon : _ -> True
      _ -> False

-- | Whether the token the parser is at is spelled this way.
atText :: String -> P Bool
atText s = gets $ \p -> case stream p of
  Next l _ _ -> tokenText (lexemeToken l) == Text.pack s
  End _ -> False

-- | How the next so many tokens are spelled.
spellings :: Int -> P [String]
spellings n = gets (go n . stream)
  where
    go 0 _ = []
    go k (Next l rest _) = Text.unpack (tokenText (lexemeToken l)) : go (k - 1 :: Int) rest
    go _ (End _) = []

atMinus :: P Bool
atMinus = (&&) <$> at VarSym <*> atText "-"

-- Blocks ----------------------------------------------------------------------

-- | A braced, semicolon-separated block of items, each starting with one
-- of some kinds of token. Gives how many items it held.
--
-- A block opened by the layout pass ends at the first token that cannot
-- carry it on: that is 'expect' at work. In a block that may not be cut
-- short, the file's own, such a token is an error, skipped to the next
-- item.
block :: Bool -> [SyntaxKind] -> P () -> P Int
block closable starts item = do
  k <- nth 0
  if k `notElem` [LBrace, VLBrace]
    then 0 <$ problem "expected a block"
    else advance >> go (if k == VLBrace then VRBrace else RBrace) 0
  where
    go :: SyntaxKind -> Int -> P Int
    go closer n = do
      k <- nth 0
      case () of
        _
          | k == closer -> n <$ advance
          | k `elem` semis -> advance >> go closer n
          | k `elem` starts -> do
              before <- gets pos
              item
              after <- gets pos
              when (before == after) (advanceWithError "unexpected token")
              separated <- atAny (closer : semis)
              if separated then go closer (n + 1) else finish closer (n + 1)
          | otherwise -> finish closer n

    -- The next token is neither the end of the block nor the start of an
    -- item.
    finish closer n
      | closable = do
          closed <- expect closer
          if closed then pure n else skip closer n
      | otherwise = problem "unexpected token" >> skip closer n

    -- Skip to the next item, as one error.
    skip closer n = do
      m <- open
      skipItem closer (0 :: Int)
      close_ m ErrorNode
      done <- eof
      if done then pure n else go closer n

    skipItem closer depth = do
      k <- nth 0
      case () of
        _
          | k == Eof -> pure ()
          | depth == 0 && k `elem` closer : semis -> pure ()
          | k `elem` [LBrace, VLBrace] -> advance >> skipItem closer (depth + 1)
          | k `elem` [RBrace, VRBrace] && depth > 0 -> advance >> skipItem closer (depth - 1)
          | otherwise -> advance >> skipItem closer depth

-- | A block that must hold something.
block1 :: String -> [SyntaxKind] -> P () -> P ()
block1 what starts item = do
  n <- block True starts item
  when (n == 0) (problem ("expected " ++ what))

patStarts, atomStarts, declStarts :: [SyntaxKind]
patStarts = [VarId, Underscore, Tilde, ConId, QConId, IntegerLit, FloatLit, CharLit, StringLit, LParen, LBracket, PrefixMinus]
atomStarts = [VarId, QVarId, ConId, QConId, IntegerLit, FloatLit, CharLit, StringLit, LParen, LBracket, PrefixMinus]
-- A binding may be marked strict, with BangPatterns.
declStarts = [KwInfix, KwInfixl, KwInfixr, Bang] ++ patStarts

closers :: [SyntaxKind]
closers = [RBrace, VRBrace]

-- | The tokens up to the end of the current declaration, brackets kept
-- whole.
restOfDecl :: P ()
restOfDecl = do
  k <- nth 0
  case () of
    _
      | k `elem` Eof : semis ++ closers ++ [RParen, RBracket] -> pure ()
      | k `elem` [LParen, LBracket] -> balanced >> restOfDecl
      | otherwise -> advance >> restOfDecl

-- | A @(...)@ or @[...]@ group, as its tokens.
balanced :: P ()
balanced = advance >> go
  where
    go = do
      k <- nth 0
      case () of
        _
          | k `elem` [RParen, RBracket] -> advance
          | k `elem` [LParen, LBracket] -> balanced >> go
          | k `elem` Eof : semis ++ closers -> problem "expected the bracket to be closed"
          | otherwise -> advance >> go

-- Declarations ----------------------------------------------------------------

sourceFile :: P ()
sourceFile = do
  m <- open
  header <- at KwModule
  when header $ do
    h <- open
    advance
    expectAny [ConId, QConId] "a module name"
    exports <- at LParen
    when exports (namespaces >> balanced)
    expect_ KwWhere
    close_ h ModuleHeader
  _ <- block False ([KwData, KwNewtype, KwType, KwImport, KwClass, KwInstance, KwForeign, KwDefault, KwDeriving] ++ declStarts) topDecl
  leftovers "expected a declaration"
  close_ m Module

-- | In a list of what is imported or exported, the word @type@ before a
-- name says the name is a type's: allowed if ExplicitNamespaces is on.
namespaces :: P ()
namespaces = do
  -- Which TypeOperators allows too, since an operator may be a type's.
  allowed <- (||) <$> on ExplicitNamespaces <*> on TypeOperators
  kinds <- ahead 0
  when (not allowed && KwType `elem` takeWhile (`notElem` Eof : KwWhere : semis) kinds) $
    problem "`type` before a name in a list of names is allowed only if ExplicitNamespaces is on"

-- | Whatever is left is an error, all of it one.
leftovers :: String -> P ()
leftovers message = do
  done <- eof
  unless done $ do
    m <- open
    problem message
    whileM (not <$> eof) advance
    close_ m ErrorNode

topDecl :: P ()
topDecl = do
  k <- nth 0
  case k of
    KwData -> dataDecl
    KwNewtype -> dataDecl
    KwType -> do
      m <- open
      advance
      kinds <- ahead 0
      -- The kind of a type, said apart from the type.
      standalone <- on StandaloneKindSignatures
      roles <- on RoleAnnotations
      role <- atText "role"
      families <- on TypeFamilies
      family <- atText "family"
      case kinds of
        -- A function from types to a type: declared, with perhaps all its
        -- equations; or one more equation of one that is open to them.
        VarId : next : _ | families && family && next `elem` [ConId, LParen] -> advance >> typeFamily m
        KwInstance : _ | families -> advance >> typeEquation >> close_ m TypeInstanceDecl
        -- What a type's parameters are to it, which is said and not used.
        VarId : next : _ | roles && role && next `elem` [ConId, LParen] -> restOfDecl >> close_ m RoleDecl
        _ : DoubleColon : _ | standalone -> do
          expect_ ConId
          advance
          kind
          close_ m KindSigDecl
        LParen : _ : RParen : DoubleColon : _ | standalone -> do
          replicateM_ 4 advance
          kind
          close_ m KindSigDecl
        _ -> do
          typeHead
          expect_ Equals
          type_
          close_ m TypeDecl
    KwImport -> do
      -- Where the word qualified goes is all the parser needs to know of
      -- an import, to say so if it is somewhere that was not asked for.
      after <- on ImportQualifiedPost
      spelled <- spellings 3
      when (not after && drop 2 spelled == ["qualified"]) (problem "`qualified` goes before the module's name, unless ImportQualifiedPost is on")
      namespaces
      packages <- on PackageImports
      kinds <- ahead 0
      when (not packages && StringLit `elem` take 3 kinds) (problem "an import names the package its module is from only if PackageImports is on")
      wholeDecl ImportDecl
    KwClass -> classDecl ClassDecl
    KwInstance -> classDecl InstanceDecl
    -- An instance derived apart from its type's declaration, with
    -- perhaps a word for how.
    KwDeriving -> do
      m <- open
      allowed <- on StandaloneDeriving
      unless allowed (problem "an instance is derived apart from its type's declaration only if StandaloneDeriving is on")
      advance
      how <- (||) <$> at KwNewtype <*> ((&&) <$> at VarId <*> ((||) <$> atText "stock" <*> atText "anyclass"))
      when how advance
      expect_ KwInstance
      qualifiedType
      close_ m StandaloneDerivingDecl
    KwDefault -> do
      m <- open
      advance
      expect_ LParen
      empty <- at RParen
      unless empty (type_ >> whileM (eat Comma) type_)
      expect_ RParen
      close_ m DefaultDecl
    _
      | k == KwForeign -> do
          m <- open
          advance
          imported <- at KwImport
          exported <- atText "export"
          if not (imported || exported)
            then do
              problem "a foreign declaration is an import or an export"
              restOfDecl
              close_ m ErrorNode
            else do
              advance
              -- The calling convention, and whether the call may come back.
              whileM ((&&) <$> at VarId <*> ((`notElem` [DoubleColon, Comma]) <$> nth 1)) advance
              _ <- eat StringLit
              binder
              expect_ DoubleColon
              type_
              close_ m (if imported then ForeignDecl else ForeignExportDecl)
      | k == VarId -> do
          -- The word pattern is a name like any other, but for where it
          -- starts a declaration and what follows could not be the
          -- arguments of a function of that name.
          synonyms <- on PatternSynonyms
          word <- atText "pattern"
          kinds <- ahead 1
          let heads = case kinds of
                ConId : _ -> True
                LParen : op : RParen : _ -> op `elem` [ConSym, Colon]
                VarId : op : _ -> op `elem` [ConSym, Backquote]
                _ -> False
          if synonyms && word && heads then patternSynonym else localDecl
      | otherwise -> localDecl
  where
    wholeDecl kind = do
      m <- open
      advance
      restOfDecl
      close_ m kind

-- | A pattern synonym, or the type of one: after the word, the synonym
-- applied to variables, or between two, and the pattern it stands for.
-- With @=@ the synonym is an expression too, and with @<-@ a pattern only.
patternSynonym :: P ()
patternSynonym = do
  m <- open
  advance
  kinds <- ahead 0
  if stoppedAt (scan [DoubleColon, Equals, LeftArrow] kinds) == DoubleColon
    then restOfDecl >> close_ m PatSynSig
    else do
      case kinds of
        ConId : _ -> advance >> whileM (at VarId) advance
        LParen : _ -> replicateM_ 3 advance >> whileM (at VarId) advance
        _ -> do
          expect_ VarId
          quoted <- eat Backquote
          if quoted then expect_ ConId >> expect_ Backquote else expectAny [ConSym] "a constructor operator"
          expect_ VarId
      expectAny [Equals, LeftArrow] "= or <-"
      pat
      -- After a where, how the synonym is built, as equations of it,
      -- where its pattern does not say.
      explicit <- at KwWhere
      when explicit $ do
        w <- open
        advance
        _ <- block True declStarts localDecl
        close_ w WhereClause
      close_ m PatSynDecl

-- | A class or an instance: a head, which is a type with perhaps a context
-- before it, and then perhaps a @where@ and declarations.
classDecl :: SyntaxKind -> P ()
classDecl kind = do
  m <- open
  advance
  qualifiedType
  -- After a bar, which of a class's variables say what others are.
  dependent <- at Bar
  when dependent $ do
    f <- open
    allowed <- on FunctionalDependencies
    unless allowed (problem "a class says which of its variables decide others only if FunctionalDependencies is on")
    advance
    whileM (atAny [VarId, RightArrow, Comma]) advance
    close_ f FunDeps
  wheres <- at KwWhere
  when wheres $ do
    w <- open
    advance
    _ <- block True (KwType : declStarts) member
    close_ w WhereClause
  close_ m kind
  where
    -- What a class or an instance has in it: definitions and signatures,
    -- and with TypeFamilies a type that goes with the class, declared in
    -- the class and given an equation in each instance.
    member = do
      associated <- at KwType
      if not associated
        then localDecl
        else do
          a <- open
          allowed <- on TypeFamilies
          unless allowed (problem "a class has a type that goes with it only if TypeFamilies is on")
          advance
          worded <- (||) <$> at KwInstance <*> ((&&) <$> ((&&) <$> at VarId <*> atText "family") <*> ((`elem` [ConId, LParen]) <$> nth 1))
          when worded advance
          kinds <- ahead 0
          if stoppedAt (scan [Equals] kinds) == Equals
            then typeEquation >> close_ a TypeInstanceDecl
            else typeFamily a

dataDecl :: P ()
dataDecl = do
  m <- open
  advance
  -- A context, which says nothing the constructors' uses do not.
  kinds <- ahead 0
  when (stoppedAt (scan [DoubleArrow, Equals] kinds) == DoubleArrow) (btype >> expect_ DoubleArrow)
  typeHead
  -- Its kind may be said in place of some of its variables.
  kinded <- (&&) <$> on KindSignatures <*> at DoubleColon
  when kinded (advance >> kind)
  -- The constructors, each written as one, or after a where each given by
  -- its type.
  typed <- at KwWhere
  if typed
    then do
      allowed <- (||) <$> on GADTSyntax <*> on GADTs
      unless allowed (problem "constructors are given by their types, after `where`, only if GADTSyntax is on")
      advance
      void (block True [ConId, LParen] typedCon)
    else do
      defined <- eat Equals
      when defined $ do
        conDecl
        whileM (eat Bar) conDecl
  -- The classes to derive instances of: after the word, which may come
  -- more than once, perhaps how, a class or several in parentheses, and
  -- with DerivingVia perhaps the type whose instances they are to be.
  whileM (at KwDeriving) $ do
    d <- open
    advance
    how <- (||) <$> at KwNewtype <*> ((&&) <$> at VarId <*> ((||) <$> atText "stock" <*> atText "anyclass"))
    when how advance
    several <- at LParen
    if several then balanced else expectAny [ConId, QConId] "a class"
    via <- (&&) <$> on DerivingVia <*> ((&&) <$> at VarId <*> atText "via")
    when via (advance >> atype)
    close_ d Deriving
  close_ m DataDecl
  where
    conDecl = do
      c <- open
      -- With ExistentialQuantification a constructor may have type
      -- variables of its own, named after the word forall, and a context.
      existential <- on ExistentialQuantification
      named <- (&&) <$> at VarId <*> atText "forall"
      when (existential && named) $ do
        f <- open
        advance
        binders
        dot <- (&&) <$> at VarSym <*> atText "."
        if dot then advance else problem "expected a . after the variables of a forall"
        close_ f ConForall
      before <- ahead 0
      when (existential && stoppedAt (scan [DoubleArrow, Bar, KwDeriving] before) == DoubleArrow) $ do
        x <- open
        btype
        expect_ DoubleArrow
        close_ x ConContext
      kinds <- ahead 0
      let strictly p = (atStrict >>= (`when` advance)) >> p
      if stoppedAt (scan [Bar, KwDeriving, ConSym, Backquote] kinds) `elem` [ConSym, Backquote]
        then strictly btype >> operator >> strictly btype
        else do
          -- Its name, which may be an operator in parentheses.
          case kinds of
            LParen : op : RParen : _ | op `elem` [ConSym, Colon] -> replicateM_ 3 advance
            _ -> expect_ ConId
          record <- eat LBrace
          if record
            then do
              empty <- at RBrace
              unless empty (fieldDecl >> whileM (eat Comma) fieldDecl)
              expect_ RBrace
            else whileM atField (strictly atype)
      close_ c ConDecl
    -- @C :: a -> T a@, or with fields named @C :: { f :: a } -> T a@.
    typedCon = do
      c <- open
      kinds <- ahead 0
      case kinds of
        LParen : op : RParen : _ | op `elem` [ConSym, Colon] -> replicateM_ 3 advance
        _ -> expect_ ConId
      expect_ DoubleColon
      -- Its type variables may be named.
      named <- (&&) <$> at VarId <*> atText "forall"
      when named $ do
        f <- open
        advance
        binders
        dot <- (&&) <$> at VarSym <*> atText "."
        if dot then advance else problem "expected a . after the variables of a forall"
        close_ f ConForall
      rest <- ahead 0
      when (stoppedAt (scan [DoubleArrow] rest) == DoubleArrow) $ do
        x <- open
        btype
        expect_ DoubleArrow
        close_ x ConContext
      record <- eat LBrace
      if record
        then do
          empty <- at RBrace
          unless empty (fieldDecl >> whileM (eat Comma) fieldDecl)
          expect_ RBrace
          expect_ RightArrow
          btype' >>= made
        else arguments
      close_ c ConDecl
    -- The types of the fields, each before an arrow, and then the type
    -- that is made.
    arguments = do
      atStrict >>= (`when` advance)
      t <- btype'
      more <- eat RightArrow
      if more then arguments else made t
    made t = openBefore t >>= (`close_` GadtResult)
    fieldDecl = do
      f <- open
      fieldName
      whileM (eat Comma) fieldName
      expect_ DoubleColon
      atStrict >>= (`when` advance)
      -- With RankNTypes a field may be for all types its variables
      -- could be.
      ranked <- (&&) <$> on RankNTypes <*> ((&&) <$> at VarId <*> atText "forall")
      if ranked then qualifiedType else type_
      close_ f FieldDecl
    -- The Report lets the mark stand apart from its type.
    -- With StrictData a field may be marked the other way, as lazy.
    atStrict = do
      lazy <- (&&) <$> on StrictData <*> at Tilde
      strict <- (||) <$> at Bang <*> ((&&) <$> at VarSym <*> atText "!")
      pure (lazy || strict)
    atField = do
      strict <- atStrict
      (strict ||) <$> atAny [ConId, QConId, VarId, LParen, LBracket]

-- | The rest of the declaration of a type family, after the words: its
-- name and variables, perhaps its kind, and perhaps after a where the
-- equations that are all it has.
typeFamily :: MarkOpened -> P ()
typeFamily m = do
  typeHead
  kinded <- at DoubleColon
  when kinded (advance >> kind)
  closed <- at KwWhere
  when closed $ do
    w <- open
    advance
    _ <- block True typeStarts (open >>= \e -> typeEquation >> close_ e TypeEquation)
    close_ w WhereClause
  close_ m TypeFamilyDecl

-- | @F Int [a] = a@
typeEquation :: P ()
typeEquation = type_ >> expect_ Equals >> type_

-- | The head of a declaration of a type: its name and then its
-- variables, or with TypeOperators an operator between two.
typeHead :: P ()
typeHead = do
  operators <- on TypeOperators
  kinds <- ahead 0
  let infixed = case dropBinder kinds of
        Just (op : _) -> op `elem` [ConSym, VarSym, Backquote]
        _ -> False
      dropBinder = \case
        VarId : rest -> Just rest
        LParen : VarId : DoubleColon : rest -> Just (drop 1 (dropWhile (/= RParen) rest))
        _ -> Nothing
  case kinds of
    _ | operators && infixed -> do
      binder'
      quoted <- eat Backquote
      if quoted then expect_ ConId >> expect_ Backquote else advance
      binder'
    LParen : op : RParen : _ | operators && op `elem` [ConSym, VarSym] -> replicateM_ 3 advance >> binders
    _ -> expect_ ConId >> binders
  where
    binder' = do
      plain <- at VarId
      if plain then advance else kindedBinder

-- | The type variables a declaration or a forall names, each of which
-- may be given a kind with KindSignatures.
binders :: P ()
binders = whileM atBinder $ do
  plain <- at VarId
  if plain then advance else kindedBinder
  where
    atBinder = do
      kinded <- on KindSignatures
      kinds <- ahead 0
      pure $ case kinds of
        VarId : _ -> True
        LParen : VarId : DoubleColon : _ -> kinded
        _ -> False

-- | @(a :: kind)@
kindedBinder :: P ()
kindedBinder = do
  b <- open
  expect_ LParen
  expect_ VarId
  expect_ DoubleColon
  kind
  expect_ RParen
  close_ b KindedBinder

-- | A kind: stars, or the word Type, with arrows between and in
-- parentheses.
kind :: P ()
kind = do
  m <- open
  go
  close_ m KindSig
  where
    go = do
      k <- nth 0
      star <- (&&) <$> at VarSym <*> atText "*"
      case () of
        _
          -- A kind may be a data type's name, a variable, or a list's.
          | star || k `elem` [ConId, QConId, RightArrow, VarId] -> advance >> go
          | k == LParen -> advance >> go >> expect_ RParen >> go
          | k == LBracket -> advance >> go >> expect_ RBracket >> go
          | otherwise -> pure ()

-- | Do something for as long as a test holds. The test may consume.
whileM :: P Bool -> P () -> P ()
whileM test body = do
  ok <- test
  when ok (body >> whileM test body)

-- | A declaration that may appear in @let@ and @where@.
localDecl :: P ()
localDecl = do
  fixity <- atAny [KwInfix, KwInfixl, KwInfixr]
  if fixity
    then do
      m <- open
      advance
      _ <- eat IntegerLit
      operator
      whileM (eat Comma) operator
      close_ m FixityDecl
    else do
      m <- open
      shape <- declShape
      case shape of
        Signature -> do
          binder
          whileM (eat Comma) binder
          expect_ DoubleColon
          qualifiedType
          close_ m TypeSig
        Function -> do
          binder
          whileM atPatStart apat
          rhs Equals
          close_ m FunClause
        InfixFunction -> do
          bpat
          operator
          bpat
          rhs Equals
          close_ m InfixClause
        ParenFunction -> do
          advance
          bpat
          operator
          bpat
          expect_ RParen
          whileM atPatStart apat
          rhs Equals
          close_ m InfixClause
        PatternBinding -> do
          pat
          rhs Equals
          close_ m PatBind

-- | The name a clause or signature is for: a variable, or an operator in
-- parentheses.
binder :: P ()
binder = do
  m <- open
  parenthesized <- at LParen
  if parenthesized
    then advance >> expectAny [VarSym, QVarSym] "an operator" >> expect_ RParen
    else expectAny [VarId] "a name"
  close_ m Binder

-- | The right-hand side of an equation (after @=@) or of a case
-- alternative (after @->@).
rhs :: SyntaxKind -> P ()
rhs = rhsWith True

-- | The alternatives of an @if@ with guards: a right-hand side that is
-- all guards, and has no @where@ of its own.
guards :: SyntaxKind -> P ()
guards = rhsWith False

rhsWith :: Bool -> SyntaxKind -> P ()
rhsWith whole sep = do
  m <- open
  guarded <- at Bar
  if guarded
    then whileM (at Bar) $ do
      g <- open
      advance
      qualifier
      whileM (eat Comma) qualifier
      expect_ sep
      expr
      close_ g GuardedRhs
    else expect_ sep >> expr
  wheres <- (whole &&) <$> at KwWhere
  when wheres $ do
    w <- open
    advance
    _ <- block True declStarts localDecl
    close_ w WhereClause
  close_ m Rhs

-- Types -----------------------------------------------------------------------

typeStarts :: [SyntaxKind]
typeStarts = [VarId, ConId, QConId, LParen, LBracket, Underscore]

-- | A type as a signature gives it: perhaps with a context before a @=>@,
-- which only looking ahead can tell from the type itself.
qualifiedType :: P ()
qualifiedType = do
  -- With ExplicitForAll its variables may be named first, after the word
  -- forall, which is a name like any other everywhere else.
  explicit <- (&&) <$> ((||) <$> on ExplicitForAll <*> on RankNTypes) <*> ((&&) <$> at VarId <*> atText "forall")
  if not explicit
    then contextual
    else do
      m <- open
      advance
      binders
      dot <- (&&) <$> at VarSym <*> atText "."
      if dot then advance else problem "expected a . after the variables of a forall"
      contextual
      close_ m TyForall
  where
    contextual = do
      kinds <- ahead 0
      if stoppedAt (scan [DoubleArrow, KwWhere] kinds) /= DoubleArrow
        then type_
        else do
          m <- open
          btype
          expect_ DoubleArrow
          type_
          close_ m TyQual

-- | Function types, which group to the right.
type_ :: P ()
type_ = do
  argument <- operands
  function <- at RightArrow
  when function $ do
    m <- openBefore argument
    advance
    type_
    close_ m TyFun

-- | With TypeOperators, types with operators between them, which bind
-- tighter than the arrow does. Which binds tighter of two of them is
-- for whoever knows their fixities.
operands :: P MarkClosed
operands = do
  first <- btype'
  more <- atTypeOperator
  if not more
    then pure first
    else do
      m <- openBefore first
      whileM atTypeOperator (operator >> btype)
      close m TyInfix
  where
    atTypeOperator = do
      operators <- on TypeOperators
      -- That two types are the same is written with an operator too.
      equalities <- on GADTs
      kinds <- ahead 0
      spelled <- spellings 1
      let allowed = operators || (equalities && spelled == ["~"])
      pure $ allowed && case kinds of
        ConSym : _ -> True
        QConSym : _ -> True
        Backquote : k : Backquote : _ -> k `elem` [ConId, QConId]
        VarSym : _ -> spelled `notElem` [["."], ["!"], ["*"]]
        _ -> False

btype :: P ()
btype = void btype'

-- | A type applied to types, or just the one.
btype' :: P MarkClosed
btype' = do
  starts <- typeStarting
  if not starts
    then missing "expected a type"
    else do
      first <- atype'
      more <- typeStarting
      if not more
        then pure first
        else do
          m <- openBefore first
          whileM typeStarting atype
          close m TyApp

-- | Whether a type starts here. With DataKinds a number or a string is
-- one.
typeStarting :: P Bool
typeStarting = (||) <$> atAny typeStarts <*> ((&&) <$> on DataKinds <*> atAny [IntegerLit, StringLit])

atype :: P ()
atype = void atype'

atype' :: P MarkClosed
atype' = do
  kinds <- ahead 0
  m <- open
  case kinds of
    VarId : _ -> advance >> close m TyVar
    Underscore : _ -> advance >> close m TyWild
    k : _ | k `elem` [IntegerLit, StringLit] -> advance >> close m TyLit
    k : _ | k `elem` [ConId, QConId] -> advance >> close m TyCon
    LParen : RParen : _ -> advance >> advance >> close m TyTuple
    -- A constructor with no name of its own: a tuple's, or the arrow.
    LParen : Comma : _ -> advance >> whileM (eat Comma) (pure ()) >> expect_ RParen >> close m TyCon
    LParen : RightArrow : RParen : _ -> advance >> advance >> advance >> close m TyCon
    -- An operator that is a type's name, by itself.
    LParen : op : RParen : _ | op `elem` [ConSym, QConSym, VarSym] -> advance >> advance >> advance >> close m TyCon
    LParen : _ -> do
      advance
      -- With RankNTypes a type in parentheses may be for all types its
      -- variables could be: a forall, and perhaps a context.
      ranked <- (&&) <$> on RankNTypes <*> ((&&) <$> at VarId <*> atText "forall")
      if ranked then qualifiedType else type_
      tuple <- at Comma
      kinded <- (&&) <$> on KindSignatures <*> at DoubleColon
      case () of
        _
          | tuple -> whileM (eat Comma) type_ >> expect_ RParen >> close m TyTuple
          | kinded -> advance >> kind >> expect_ RParen >> close m TyKinded
          | otherwise -> expect_ RParen >> close m TyParen
    LBracket : RBracket : _ -> advance >> advance >> close m TyCon
    LBracket : _ -> do
      -- With a tick before it, a list of types, which is a type.
      promoted <- atText "'["
      advance
      type_
      if promoted
        then whileM (eat Comma) type_ >> expect_ RBracket >> close m TyPromotedList
        else expect_ RBracket >> close m TyList
    _ -> problem "expected a type" >> close m ErrorNode

-- Expressions -----------------------------------------------------------------

operatorKinds :: [SyntaxKind]
operatorKinds = [VarSym, ConSym, Colon, QVarSym, QConSym, Backquote, Bang, Tilde]

-- | An infix operator: a symbol, or a name in backquotes.
operator :: P ()
operator = do
  m <- open
  quoted <- at Backquote
  if quoted
    then advance >> expectAny [VarId, ConId, QVarId, QConId] "a name" >> expect_ Backquote
    else expectAny operatorKinds "an operator"
  close_ m Operator

-- | Whether an operator is next that has an operand after it: in @(x +)@
-- the @+@ is the section's, not the expression's.
atInfixOperator :: P Bool
atInfixOperator = do
  kinds <- ahead 0
  pure $ case kinds of
    Backquote : _ : Backquote : next : _ -> next /= RParen
    k : next : _ -> k `elem` operatorKinds && next /= RParen
    _ -> False

expr :: P ()
expr = void expr'

-- | An expression, perhaps said to have a type.
expr' :: P MarkClosed
expr' = do
  e <- infixExpr
  annotated <- at DoubleColon
  if not annotated
    then pure e
    else do
      m <- openBefore e
      advance
      qualifiedType
      close m TypedExpr

-- | Operands and operators in a row. The tree keeps them flat, since which
-- operator binds tighter depends on fixities the parser does not know.
infixExpr :: P MarkClosed
infixExpr = do
  negative <- atMinus
  if negative
    then do
      m <- open
      negations
      _ <- exp10
      infixTail m
    else do
      first <- exp10
      more <- atInfixOperator
      if more then openBefore first >>= infixTail else pure first
  where
    infixTail m = do
      whileM atInfixOperator (operator >> negations >> void exp10)
      close m InfixExpr
    negations = whileM atMinus $ do
      n <- open
      advance
      close_ n Negation

exp10 :: P MarkClosed
exp10 = do
  k <- nth 0
  next <- nth 1
  cased <- (&& (k, next) == (Backslash, KwCase)) <$> on LambdaCase
  multi <- (&& (k, next) == (KwIf, Bar)) <$> on MultiWayIf
  case k of
    Backslash | cased -> do
      m <- open
      advance
      advance
      alternatives
      close m LamCaseExpr
    Backslash -> do
      m <- open
      advance
      patterns <- atPatStart
      unless patterns (problem "expected a pattern")
      whileM atPatStart apat
      expect_ RightArrow
      expr
      close m LamExpr
    KwLet -> do
      m <- open
      advance
      _ <- block True declStarts localDecl
      expect_ KwIn
      expr
      close m LetExpr
    KwIf | multi -> do
      m <- open
      advance
      guards RightArrow
      close m MultiIfExpr
    KwIf -> do
      m <- open
      advance
      expr
      branch KwThen
      expr
      branch KwElse
      expr
      close m IfExpr
    KwCase -> do
      m <- open
      advance
      expr
      expect_ KwOf
      alternatives
      close m CaseExpr
    KwDo -> do
      m <- open
      advance
      block1 "a statement" ([Backslash, KwLet, KwIf, KwCase, KwDo, VarSym, Underscore, Tilde] ++ atomStarts) stmt
      close m DoExpr
    _ -> application
  where
    -- Inside a do block, layout puts a semicolon before an aligned then or
    -- else.
    branch keyword = do
      separated <- (&&) <$> atAny semis <*> ((== keyword) <$> nth 1)
      when separated advance
      expect_ keyword
    -- A case's alternatives, of which there may be none only if that is
    -- asked for.
    alternatives = do
      none <- on EmptyCase
      if none then void (block True (VarSym : Bang : patStarts) alt) else block1 "a case alternative" (VarSym : Bang : patStarts) alt
    alt = do
      m <- open
      pat
      rhs RightArrow
      close_ m Alt

-- | A function applied to arguments, or just the one thing.
application :: P MarkClosed
application = do
  starts <- atAtomStart
  if not starts
    then missing "expected an expression"
    else do
      first <- recordAtom
      more <- atArgument
      if not more
        then pure first
        else do
          m <- openBefore first
          whileM atArgument argument
          close m AppExpr
  where
    -- With BlockArguments a do, a lambda, a case, an if or a let is an
    -- argument as it stands, and takes all that follows it.
    blocks = [KwDo, Backslash, KwCase, KwIf, KwLet]
    atArgument = do
      allowed <- on BlockArguments
      typed <- atTypeArgument
      (\atomic blocky -> typed || atomic || blocky) <$> atAtomStart <*> ((allowed &&) <$> atAny blocks)
    -- With TypeApplications an argument may be a type, after an @: what
    -- one of the function's type variables is.
    atTypeArgument = do
      allowed <- on TypeApplications
      marked <- at At
      next <- nth 1
      pure (allowed && marked && next `elem` typeStarts)
    argument = do
      typed <- atTypeArgument
      blocky <- atAny blocks
      case () of
        _
          | typed -> do
              t <- open
              advance
              atype
              close_ t TypeArg
          | blocky -> void exp10
          | otherwise -> void recordAtom

-- | An atom, and any braces of field bindings after it, which bind
-- tighter than application does.
recordAtom :: P MarkClosed
recordAtom = atom >>= go
  where
    go subject = do
      braced <- at LBrace
      if not braced
        then pure subject
        else do
          m <- openBefore subject
          fieldBinds expr
          close m RecordExpr >>= go

-- | The name of a field: a variable, or an operator in parentheses.
fieldName :: P ()
fieldName = do
  parenthesized <- at LParen
  if parenthesized
    then advance >> expectAny [VarSym, QVarSym] "an operator" >> expect_ RParen
    else expectAny [VarId, QVarId] "a field"

-- | @{ field = value, ... }@
fieldBinds :: P () -> P ()
fieldBinds value = do
  expect_ LBrace
  empty <- at RBrace
  unless empty (bind >> whileM (eat Comma) bind)
  expect_ RBrace
  where
    -- With NamedFieldPuns a field by itself is the variable of its name,
    -- and with RecordWildCards two dots are all the fields not named.
    bind = do
      f <- open
      wild <- at DotDot
      if wild
        then do
          allowed <- on RecordWildCards
          unless allowed (problem "`..` stands for the fields not named only if RecordWildCards is on")
          advance
          close_ f FieldWild
        else do
          fieldName
          puns <- on NamedFieldPuns
          given <- at Equals
          when (given || not puns) (expect_ Equals >> value)
          close_ f FieldBind

atAtomStart :: P Bool
atAtomStart = atAny atomStarts

atom :: P MarkClosed
atom = do
  kinds <- ahead 0
  m <- open
  case kinds of
    k : _ | k `elem` [VarId, QVarId] -> advance >> close m VarExpr
    k : _ | k `elem` [ConId, QConId] -> advance >> close m ConExpr
    k : _ | k `elem` [IntegerLit, FloatLit, CharLit, StringLit] -> advance >> close m LitExpr
    -- With LexicalNegation a minus that touches what follows it and not
    -- what comes before is that thing's sign, and binds as tightly as
    -- anything does.
    PrefixMinus : _ -> do
      n <- open
      advance
      close_ n Negation
      _ <- recordAtom
      close m InfixExpr
    LParen : RParen : _ -> advance >> advance >> close m TupleExpr
    LParen : op : RParen : _
      | op `elem` [VarSym, QVarSym] -> advance >> advance >> advance >> close m VarExpr
      | op `elem` [ConSym, Colon, QConSym] -> advance >> advance >> advance >> close m ConExpr
    -- A tuple's constructor, by itself: its commas. With TupleSections,
    -- commas with anything among them are a tuple with places left out.
    LParen : Comma : rest | RParen : _ <- dropWhile (== Comma) rest -> advance >> whileM (eat Comma) (pure ()) >> expect_ RParen >> close m ConExpr
    LParen : Comma : _ -> do
      sections <- on TupleSections
      advance
      if sections
        then hole >> places >> expect_ RParen >> close m TupleSection
        else whileM (eat Comma) (pure ()) >> expect_ RParen >> close m ConExpr
    LParen : op : _ | op `elem` operatorKinds -> do
      advance
      -- A minus is not the start of a section: it negates what follows.
      -- With LexicalNegation it is one, when it stands apart from that.
      minus <- (&&) <$> atMinus <*> (not <$> on LexicalNegation)
      if minus then parenthesised m else operator >> expr >> expect_ RParen >> close m SectionR
    LParen : _ -> advance >> parenthesised m
    LBracket : RBracket : _ -> advance >> advance >> close m ListExpr
    LBracket : _ -> do
      advance
      expr
      k <- nth 0
      let upTo kind = do
            open' <- at RBracket
            unless open' expr
            expect_ RBracket
            close m kind
      case k of
        DotDot -> advance >> upTo RangeExpr
        Bar -> do
          advance
          qualifier
          whileM (eat Comma) qualifier
          -- With ParallelListComp, more qualifiers after another bar are
          -- gone through beside these.
          whileM (at Bar) $ do
            allowed <- on ParallelListComp
            unless allowed (problem "a list comprehension has one bar, unless ParallelListComp is on")
            b <- open
            advance
            qualifier
            whileM (eat Comma) qualifier
            close_ b ListCompBranch
          expect_ RBracket
          close m ListComp
        Comma -> do
          advance
          expr
          stepped <- eat DotDot
          if stepped then upTo RangeStepExpr else whileM (eat Comma) expr >> expect_ RBracket >> close m ListExpr
        _ -> expect_ RBracket >> close m ListExpr
    _ -> problem "expected an expression" >> close m ErrorNode
  where
    -- After an opening parenthesis: an expression, a tuple, or an
    -- expression with an operator after it.
    parenthesised m = do
      expr
      k <- nth 0
      case () of
        _
          | k == Comma -> do
              sections <- on TupleSections
              if not sections
                then whileM (eat Comma) expr >> expect_ RParen >> close m TupleExpr
                else do
                  holes <- places
                  expect_ RParen
                  close m (if holes then TupleSection else TupleExpr)
          | k `elem` operatorKinds -> operator >> expect_ RParen >> close m SectionL
          | otherwise -> expect_ RParen >> close m ParenExpr

-- | A place of a tuple left out.
hole :: P ()
hole = open >>= (`close_` TupleHole)

-- | The rest of a tuple's places, each after its comma, any of which may
-- be left out. Says whether any was.
places :: P Bool
places = go False
  where
    go left = do
      more <- eat Comma
      if not more
        then pure left
        else do
          empty <- atAny [Comma, RParen]
          if empty then hole >> go True else expr >> go left

stmt :: P ()
stmt = stmtUpTo []

-- | A qualifier of a guard or a list comprehension: a statement that ends
-- at a comma, or at what ends the guard.
qualifier :: P ()
qualifier = stmtUpTo [Comma, Equals, RightArrow, Bar]

-- | A statement, given what besides the end of its line would end it.
stmtUpTo :: [SyntaxKind] -> P ()
stmtUpTo enders = do
  kinds <- ahead 0
  m <- open
  case kinds of
    KwLet : _ -> do
      advance
      _ <- block True declStarts localDecl
      body <- at KwIn
      if body
        then do
          advance
          expr
          inner <- close m LetExpr
          outer <- openBefore inner
          close_ outer ExprStmt
        else close_ m LetStmt
    _
      | stoppedAt (scan (LeftArrow : enders) kinds) == LeftArrow -> pat >> expect_ LeftArrow >> expr >> close_ m BindStmt
      | otherwise -> expr >> close_ m ExprStmt

-- Patterns --------------------------------------------------------------------

atPatStart :: P Bool
atPatStart = do
  banged <- on BangPatterns
  atAny (if banged then Bang : patStarts else patStarts)

pat :: P ()
pat = void pat'

pat' :: P MarkClosed
pat' = do
  first <- bpat'
  kinds <- ahead 0
  let between = case kinds of
        k : _ | k `elem` [Colon, ConSym, QConSym] -> 1
        Backquote : k : Backquote : _ | k `elem` [ConId, QConId] -> 3
        _ -> 0 :: Int
  if between == 0
    then pure first
    else do
      m <- openBefore first
      replicateM_ between advance
      pat
      close m InfixPat

bpat :: P ()
bpat = void bpat'

-- | A constructor applied to patterns, a negative number, or a single
-- pattern.
bpat' :: P MarkClosed
bpat' = do
  kinds <- ahead 0
  minus <- atMinus
  banged <- on BangPatterns
  let starts = if banged then Bang : patStarts else patStarts
  case kinds of
    con : next : _ | con `elem` [ConId, QConId] && next `elem` starts -> do
      m <- open
      advance
      whileM atPatStart apat
      close m ConPat
    -- A constructor that is an operator, written before its fields.
    LParen : op : RParen : next : _ | op `elem` [ConSym, QConSym, Colon] && next `elem` starts -> do
      m <- open
      replicateM_ 3 advance
      whileM atPatStart apat
      close m ConPat
    _ : number : _ | minus && number `elem` [IntegerLit, FloatLit] -> do
      m <- open
      advance
      advance
      close m NegLitPat
    _ -> apat'

apat :: P ()
apat = void apat'

apat' :: P MarkClosed
apat' = do
  kinds <- ahead 0
  m <- open
  case kinds of
    VarId : At : _ -> advance >> advance >> apat >> close m AsPat
    VarId : _ -> advance >> close m VarPat
    Underscore : _ -> advance >> close m WildPat
    Tilde : _ -> advance >> apat >> close m LazyPat
    Bang : _ -> do
      banged <- on BangPatterns
      unless banged (problem "a pattern cannot start with `!` unless BangPatterns is on")
      advance >> apat >> close m BangPat
    k : LBrace : _ | k `elem` [ConId, QConId] -> advance >> fieldBinds pat >> close m RecordPat
    k : _ | k `elem` [ConId, QConId] -> advance >> close m ConPat
    k : _ | k `elem` [IntegerLit, FloatLit, CharLit, StringLit] -> advance >> close m LitPat
    PrefixMinus : number : _ | number `elem` [IntegerLit, FloatLit] -> advance >> advance >> close m NegLitPat
    LParen : RParen : _ -> advance >> advance >> close m TuplePat
    -- An operator in parentheses: a variable, or a constructor.
    LParen : op : RParen : _
      | op == VarSym -> replicateM_ 3 advance >> close m VarPat
      | op `elem` [ConSym, QConSym, Colon] -> replicateM_ 3 advance >> close m ConPat
    LParen : _ -> do
      advance
      component
      tuple <- at Comma
      if tuple
        then whileM (eat Comma) component >> expect_ RParen >> close m TuplePat
        else expect_ RParen >> close m ParenPat
    LBracket : _ -> do
      advance
      empty <- at RBracket
      unless empty (pat >> whileM (eat Comma) pat)
      expect_ RBracket
      close m ListPat
    _ -> problem "expected a pattern" >> close m ErrorNode

-- | A pattern in parentheses or in a tuple, which with ViewPatterns may
-- be a function, an arrow, and a pattern for what the function gives.
component :: P ()
component = do
  views <- on ViewPatterns
  kinds <- ahead 0
  if views && stoppedAt (scan [RightArrow, Comma] kinds) == RightArrow
    then do
      v <- open
      expr
      expect_ RightArrow
      pat
      close_ v ViewPat
    else do
      -- With ScopedTypeVariables a pattern may be said to have a type.
      p <- pat'
      signed <- (&&) <$> on ScopedTypeVariables <*> at DoubleColon
      when signed $ do
        s <- openBefore p
        advance
        type_
        close_ s SigPat

-- The prompt ------------------------------------------------------------------

-- | An expression on its own, inside the block the layout pass wraps the
-- input in.
replExpr :: P ()
replExpr = do
  m <- open
  expectAny [LBrace, VLBrace] "an expression"
  expr
  whileM (atAny semis) advance
  expect_ VRBrace
  leftovers "unexpected token"
  close_ m ReplExpr
