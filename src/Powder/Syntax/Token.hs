-- | Tokens, and where they are.
module Powder.Syntax.Token
  ( Token (..),
    Pos (..),
    Lexeme (..),
    lexemes,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Powder.Syntax.Kind

-- | A token is a kind and the exact text it was read from. The texts of a
-- file's tokens, end to end, are the file.
data Token = Token {tokenKind :: !SyntaxKind, tokenText :: !Text}
  deriving (Eq, Show)

-- | A line and column, both from 1. Column 0 of the line after the last
-- stands for the end of the input.
data Pos = Pos {posLine :: !Int, posCol :: !Int}
  deriving (Eq, Ord)

instance Show Pos where
  show (Pos l c) = show l ++ ":" ++ show c

-- | A token that means something, with the trivia before it and its
-- position.
data Lexeme = Lexeme
  { lexemeTrivia :: [Token],
    lexemeToken :: Token,
    lexemePos :: Pos
  }

-- | Group tokens into lexemes. Also gives the trivia after the last one,
-- and the position of the end of the input.
lexemes :: [Token] -> ([Lexeme], [Token], Pos)
lexemes = go (Pos 1 1) []
  where
    go p trivia = \case
      [] -> ([], reverse trivia, Pos (posLine p + 1) 0)
      t : ts
        | isTrivia (tokenKind t) -> go (advance p t) (t : trivia) ts
        | otherwise ->
            let (ls, trailing, end) = go (advance p t) [] ts
             in (Lexeme (reverse trivia) t p : ls, trailing, end)
    advance p t = foldl' step p (Text.unpack (tokenText t))
    step (Pos l _) '\n' = Pos (l + 1) 1
    -- A tab goes to the next column that is one more than a multiple of
    -- eight, as the Report says the layout rule is to take it.
    step (Pos l c) '\t' = Pos l (((c - 1) `div` 8 + 1) * 8 + 1)
    step (Pos l c) _ = Pos l (c + 1)
