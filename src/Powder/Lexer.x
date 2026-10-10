{
{-# OPTIONS_GHC -w #-}

-- | The lexical syntax of Haskell 2010, and of the extensions to it that
-- change what a token is.
--
-- The lexer keeps everything: whitespace and comments are tokens too, and
-- text that is no token at all becomes an 'Error' token rather than a
-- failure. So the texts of the tokens, end to end, are always the input.
-- It knows nothing of layout, which is a pass of its own.
module Powder.Lexer (lexer, lexerWith) where

import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Word (Word8)
import Powder.Extensions (Extension (..), Extensions)
import Powder.Syntax.Kind
import Powder.Syntax.Token
}

%encoding "latin1"

-- Characters beyond ASCII reach the automaton as one of a few stand-in
-- bytes, by what kind of character they are: see 'classify'.
$upper    = [A-Z \x01]
$lower    = [a-z _ \x02]
$digit    = [0-9]
$octit    = [0-7]
$hexit    = [0-9 A-F a-f]
$symbol   = [\! \# \$ \% \& \* \+ \. \/ \< \= \> \? \@ \\ \^ \| \- \~ \: \x03]
$varsym1  = $symbol # \:
$idchar   = [$upper $lower $digit \' \x04 \x06]
$space    = [\  \t \n \r \f \v \x05]
$charesc  = [A-Z a-z 0-9 \^ \[ \] \\ _ \@]

@varid    = $lower $idchar*
@conid    = $upper $idchar*
@qual     = (@conid \.)+
@varsym   = $varsym1 $symbol*
@consym   = \: $symbol*
@decimal  = $digit+
-- Digits with underscores among them, for NumericUnderscores.
@spaced   = $digit+ (\_+ $digit+)*
@spacedexp = [eE] [\-\+]? @spaced
@exponent = [eE] [\-\+]? @decimal
@escape   = \\ ~$space
@gap      = \\ $space+ \\
@strbody  = ([^ \" \\ \n] | @escape | @gap)*
@charbody = [^ \' \\ \n] | \\ ~$space $charesc*

tokens :-

  $space+                                    { plain Whitespace }

  -- Dashes start a comment unless a symbol follows, which makes them, with
  -- it, an operator: the longer match wins.
  "--" \-* ([^ $symbol \n] [^ \n]*)?         { plain LineComment }
  "{-"                                       { blockComment }

  @varid                                     { identifier }
  @conid                                     { plain ConId }
  @qual @varid                               { plain QVarId }
  @qual @conid                               { plain QConId }
  -- What an operator is can depend on what touches it, as in GHC's lexer:
  -- one with something to bind to after it but not before is a prefix
  -- occurrence, the other way about a suffix one, with both a tight infix
  -- one and with neither a loose infix one. The first that holds wins.
  @varsym / { precededByClosingToken `alexAndPred` followedByOpeningToken } { operator TightInfix }
  @varsym / { followedByOpeningToken }       { operator Prefix }
  @varsym / { precededByClosingToken }       { operator Suffix }
  @varsym                                    { operator LooseInfix }
  @consym                                    { operator LooseInfix }
  @qual @varsym                              { plain QVarSym }
  @qual @consym                              { plain QConSym }

  @decimal                                   { plain IntegerLit }
  0 [oO] $octit+                             { plain IntegerLit }
  0 [xX] $hexit+                             { plain IntegerLit }
  @decimal \. @decimal @exponent?            { plain FloatLit }
  @decimal @exponent                         { plain FloatLit }

  -- Underscores in numbers, and numbers in binary.
  $digit+ (\_+ $digit+)+ / { on NumericUnderscores }                    { plain IntegerLit }
  0 [oO] \_* $octit+ (\_+ $octit+)* / { on NumericUnderscores }         { plain IntegerLit }
  0 [xX] \_* $hexit+ (\_+ $hexit+)* / { on NumericUnderscores }         { plain IntegerLit }
  @spaced \. @spaced @spacedexp? / { on NumericUnderscores }            { plain FloatLit }
  @spaced @spacedexp / { on NumericUnderscores }                        { plain FloatLit }
  0 [bB] [01]+ / { on BinaryLiterals }                                  { plain IntegerLit }
  -- Fractions in hexadecimal, with a power of two after a p.
  0 [xX] $hexit+ (\. $hexit+)? [pP] [\-\+]? @decimal / { on HexFloatLiterals } { plain FloatLit }
  0 [xX] $hexit+ \. $hexit+ / { on HexFloatLiterals }                    { plain FloatLit }
  0 [bB] \_* [01]+ (\_+ [01]+)* / { on BinaryLiterals `alexAndPred` on NumericUnderscores } { plain IntegerLit }

  \' @charbody \'                            { plain CharLit }
  \" @strbody \"                             { plain StringLit }
  \" @strbody                                { plain Error }

  \(                                         { plain LParen }
  \)                                         { plain RParen }
  \,                                         { plain Comma }
  \;                                         { plain Semicolon }
  \[                                         { plain LBracket }
  \]                                         { plain RBracket }
  \`                                         { plain Backquote }
  \{                                         { plain LBrace }
  \}                                         { plain RBrace }

{
-- | Given the input from where the token starts and how much of it the
-- pattern matched, the token's kind and how much of the input it takes.
type Action = String -> Int -> (SyntaxKind, Int)

plain :: SyntaxKind -> Action
plain k _ len = (k, len)

identifier :: Action
identifier s len = (Map.findWithDefault VarId (take len s) keywords, len)

-- | Whether an extension is on, for a rule that is only a rule if it is.
on :: Extension -> Extensions -> AlexInput -> Int -> AlexInput -> Bool
on extension extensions _ _ _ = Set.member extension extensions

-- | How an operator sits among its neighbours.
data Occurrence = Prefix | Suffix | TightInfix | LooseInfix
  deriving (Eq)

-- | An operator. Two are something else as a prefix occurrence and an
-- operator like any other otherwise: @~@ before a pattern makes it lazy,
-- and @!@ before a type makes a field strict.
operator :: Occurrence -> Action
operator occurrence s len = (kind, len)
  where
    spelled = take len s
    kind = case (spelled, occurrence) of
      ("~", Prefix) -> Tilde
      ("!", Prefix) -> Bang
      _ -> Map.findWithDefault (if take 1 s == ":" then ConSym else VarSym) spelled reservedOps

-- | Whether what is just before a token is the end of something an
-- operator could apply to: a name, a literal or a closing bracket.
precededByClosingToken :: user -> AlexInput -> Int -> AlexInput -> Bool
precededByClosingToken _ before _ _ = closes (previous before)
  where
    closes c = isAlphaNum c || c `elem` ")]\125\"'_"

-- | Whether what is just after a token is the start of something an
-- operator could apply to: a name, a literal or an opening bracket that
-- is not a comment's.
followedByOpeningToken :: user -> AlexInput -> Int -> AlexInput -> Bool
followedByOpeningToken _ _ _ after = case remaining after of
  '\123' : '-' : _ -> False
  c : _ -> isAlphaNum c || c `elem` "([\123\"'_"
  [] -> False

keywords :: Map.Map String SyntaxKind
keywords =
  Map.fromList
    [ ("case", KwCase), ("class", KwClass), ("data", KwData), ("default", KwDefault),
      ("deriving", KwDeriving), ("do", KwDo), ("else", KwElse), ("foreign", KwForeign),
      ("if", KwIf), ("import", KwImport), ("in", KwIn), ("infix", KwInfix),
      ("infixl", KwInfixl), ("infixr", KwInfixr), ("instance", KwInstance), ("let", KwLet),
      ("module", KwModule), ("newtype", KwNewtype), ("of", KwOf), ("then", KwThen),
      ("type", KwType), ("where", KwWhere), ("_", Underscore)
    ]

reservedOps :: Map.Map String SyntaxKind
reservedOps =
  Map.fromList
    [ ("..", DotDot), (":", Colon), ("::", DoubleColon), ("=", Equals), ("\\", Backslash),
      ("|", Bar), ("<-", LeftArrow), ("->", RightArrow), ("@", At), ("=>", DoubleArrow)
    ]

-- | A block comment runs to its matching close, and comments nest. One that
-- opens as a pragma does is a pragma; one that never closes is an error
-- that takes the rest of the input.
blockComment :: Action
blockComment s _ = go (1 :: Int) 2 (drop 2 s)
  where
    go 0 n _ = (if take 3 s == [open, '-', '#'] then Pragma else BlockComment, n)
    go depth n rest = case rest of
      c : '-' : more | c == open -> go (depth + 1) (n + 2) more
      '-' : c : more | c == close -> go (depth - 1) (n + 2) more
      _ : more -> go depth (n + 1) more
      [] -> (Error, n)
    -- Written by number: Alex counts the braces in this file's code.
    open = '\123'
    close = '\125'

-- | The automaton reads bytes; this gives it one per character.
data AlexInput = AlexInput {previous :: Char, remaining :: String}

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (AlexInput _ s) = case s of
  c : cs -> Just (classify c, AlexInput c cs)
  [] -> Nothing

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = previous

-- | An ASCII character is its own byte. Any other is a byte for its class,
-- as the report sorts Unicode: upper and title case letters, other
-- letters, symbols and punctuation, digits, spaces, and characters that
-- may continue a name. An ASCII control character that is not white space
-- is in no class, so that it cannot be mistaken for one of these.
classify :: Char -> Word8
classify c
  | c < '\x80' = if isControl c && c `notElem` "\t\n\r\f\v" then 0 else fromIntegral (ord c)
  | otherwise = case generalCategory c of
      UppercaseLetter -> 1
      TitlecaseLetter -> 1
      LowercaseLetter -> 2
      OtherLetter -> 2
      DecimalNumber -> 4
      ModifierLetter -> 6
      NonSpacingMark -> 6
      SpacingCombiningMark -> 6
      EnclosingMark -> 6
      LetterNumber -> 6
      OtherNumber -> 6
      Space -> 5
      _
        | isSymbol c || isPunctuation c -> 3
        | otherwise -> 0

-- | Every token of the input, in order, as the Report has tokens.
lexer :: String -> [Token]
lexer = lexerWith Set.empty

-- | Every token of the input, in order, with some extensions on.
lexerWith :: Extensions -> String -> [Token]
lexerWith extensions = go . AlexInput '\n'
  where
    go input = case alexScanUser extensions input 0 of
      AlexEOF -> []
      AlexError _ -> emit Error 1
      AlexSkip _ len -> emit Whitespace len
      AlexToken _ len action -> uncurry emit (action (remaining input) len)
      where
        emit k len =
          let (lexeme, rest) = splitAt len (remaining input)
           in Token k (Text.pack lexeme) : go (AlexInput (last (previous input : lexeme)) rest)
}
