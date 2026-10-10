-- | How the prompt reads an entry as it is typed: whether Enter submits it
-- or carries it on, how far in the next line starts, and what kind of name
-- the cursor is on. Modelled on Meadow's REPL.
module Entry
  ( needsContinuation,
    autoIndent,
    lineBreak,
    Position (..),
    position,
    wordStart,
  )
where

import Data.Char (isAlphaNum, isSpace)
import Data.Either (isRight)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Powder.Extensions (Extension (EmptyCase))
import Powder.Frontend (Settings (..), defaultSettings, parseLineWith)
import Powder.Lexer
import Powder.Syntax.Kind
import Powder.Syntax.Token

-- | One level of indentation.
unit :: Int
unit = 2

-- | Whether an entry should keep taking lines rather than be submitted.
--
-- - A @:@ command is always finished.
-- - On the first line, submit as soon as it parses, so that @1 + 2@ takes a
--   single Enter; otherwise keep editing.
-- - Once the entry spans several lines a blank line ends it, whatever it
--   holds. Short of that, it is submitted early if it parses and nothing is
--   left dangling: no open bracket, and no operator or keyword at the end
--   that wants more.
-- - A type signature parses by itself but is half a declaration, so an
--   entry that opens with one runs until the blank line, to take every
--   equation under it. So does one whose last line is a guard.
needsContinuation :: String -> Bool
needsContinuation input
  | ":" `isPrefixOf` dropWhile isSpace input = False
  | multiline && all isSpace (last rows) = False
  | any (opensSignature . snd) (take 1 coded ++ take 1 (reverse coded)) = True
  | not multiline = not parses
  | take 1 (snd (last coded)) == [Bar] = True
  | depth toks > 0 || maybe False dangling (lastOf toks) = True
  | otherwise = not parses
  where
    rows = splitLines input
    multiline = length rows > 1
    toks = tokens input
    -- The lines that hold code, each with its tokens.
    coded = [(l, ts) | l <- [1 .. length rows], let ts = [tokenKind (lexemeToken x) | x <- significant, posLine (lexemePos x) == l], not (null ts)] ++ [(0 :: Int, []) | null toks]
    (significant, _, _) = lexemes (lexer input)
    -- A case with no alternatives yet is one still being typed, whatever
    -- the language says of one that has none.
    parses = isRight (parseLineWith defaultSettings {settingsChanges = [(EmptyCase, False)]} "" input)

-- | Whether a line is a signature with nothing defined yet: names, and
-- then their type. Anything else before a @::@ is an expression said to
-- have a type.
opensSignature :: [SyntaxKind] -> Bool
opensSignature ts = names ts && Equals `notElem` ts
  where
    names = \case
      VarId : rest -> more rest
      LParen : _ : RParen : rest -> more rest
      _ -> False
    more = \case
      Comma : rest -> names rest
      DoubleColon : _ -> True
      _ -> False

splitLines :: String -> [String]
splitLines s = case break (== '\n') s of
  (l, _ : rest) -> l : splitLines rest
  (l, []) -> [l]

-- | The kinds of the tokens of some text that mean something.
tokens :: String -> [SyntaxKind]
tokens = filter (not . isTrivia) . map tokenKind . lexer

lastOf :: [a] -> Maybe a
lastOf = foldl (const Just) Nothing

-- | How many brackets are still open.
depth :: [SyntaxKind] -> Int
depth ts = length (filter (`elem` [LParen, LBracket, LBrace]) ts) - length (filter (`elem` [RParen, RBracket, RBrace]) ts)

-- | Whether a token at the end of a line wants something after it.
dangling :: SyntaxKind -> Bool
dangling k =
  k `elem` [VarSym, ConSym, QVarSym, QConSym, Colon]
    || k `elem` [Equals, RightArrow, LeftArrow, Bar, Backslash, DoubleColon, DotDot, Comma]
    || k `elem` [KwLet, KwIn, KwIf, KwThen, KwElse, KwCase, KwOf, KwDo, KwWhere]

-- | The whitespace a continuation line opens with, given what is before
-- the cursor: the indent of the line above, and a unit more if that line
-- opened something.
autoIndent :: String -> String
autoIndent before = replicate (base + if opens then unit else 0) ' '
  where
    above = fromMaybe "" (lastOf (filter (not . all isSpace) (splitLines before)))
    base = length (takeWhile (== ' ') above)
    ts = tokens above
    opens = depth ts > 0 || maybe False dangling (lastOf ts)

-- | What a line-break key puts at the cursor. Enter breaks the line only
-- while the entry is unfinished, and otherwise gives 'Nothing', to submit
-- it; the keys that ask for a line break by name always get one.
--
-- A break asked for by name after a whole line carries the entry on: after
-- the head of a definition, for the guards that stand a unit in under it,
-- and after an equation, for the next one at the margin.
lineBreak :: Bool -> String -> Int -> Maybe String
lineBreak always line pos
  | not always && not (needsContinuation line) = Nothing
  | always && null indent && not (all isSpace before) && Equals `notElem` tokens above = Just ('\n' : replicate unit ' ')
  | otherwise = Just ('\n' : indent)
  where
    before = take pos line
    indent = autoIndent before
    above = fromMaybe "" (lastOf (filter (not . all isSpace) (splitLines before)))

-- | What kind of name the cursor is on.
data Position
  = -- | An expression or a pattern: values and constructors.
    Term
  | -- | A type, in a signature or a data declaration.
    Type
  | -- | Somewhere a new name is being made up, so nothing completes.
    Naming
  deriving (Eq, Show)

-- | Where the word ending at a position starts.
wordStart :: String -> Int -> Int
wordStart line pos = pos - length (takeWhile isWordChar (reverse (take pos line)))

isWordChar :: Char -> Bool
isWordChar c = isAlphaNum c || c == '_' || c == '\''

-- | Decide what the cursor is on, given the entry before it.
position :: String -> Position
position before = case entry of
  d : rest | d `elem` [KwData, KwNewtype] -> case lastOf rest of
    _ | Equals `notElem` rest -> Naming
    Just s | s `elem` [Equals, Bar] -> Naming
    _ -> Type
  KwType : rest -> if Equals `elem` rest then Type else Naming
  _ | DoubleColon `elem` current && Equals `notElem` current -> Type
  _ -> Term
  where
    start = wordStart before (length before)
    -- The word under the cursor is not part of what came before it.
    entry = tokens (take start before)
    current = tokens (fromMaybe "" (lastOf (splitLines (take start before))))
