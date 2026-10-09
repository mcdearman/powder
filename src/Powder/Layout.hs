-- | The layout rule, as a pass between the lexer and the parser.
--
-- It adds the braces and semicolons that indentation implies, as tokens
-- with no text, so its output still spells out the input exactly.
--
-- One clause of the rule cannot be decided here. The report closes an
-- implicit block wherever the next token would otherwise be a parse error:
-- that is what ends the @let@ block in @let x = 1 in x@. Only the parser
-- knows where that is. So this pass answers both ways: at every token
-- inside an implicit block, the stream says what comes next if the block
-- goes on, and what would come next had it been closed there. The parser
-- reads the first, and takes the second when it wanted a closing brace and
-- found something else.
module Powder.Layout
  ( Stream (..),
    layout,
    kinds,
  )
where

import Data.Text qualified as Text
import Powder.Syntax.Kind
import Powder.Syntax.Token

data Stream
  = -- | The next token, the stream after it, and the stream from here as
    -- it is if the innermost block is implicit and is closed first: that
    -- stream starts with the closing brace.
    --
    -- Both streams are lazy, and have to be. The parser follows one path
    -- through them; made eagerly, every way of closing every block at
    -- every token would be worked out.
    Next Lexeme ~Stream ~(Maybe Stream)
  | -- | The end of the input, and where it is.
    End Pos

-- | The kinds of the tokens to come, if no block is closed early.
kinds :: Stream -> [SyntaxKind]
kinds = \case
  Next l rest _ -> tokenKind (lexemeToken l) : kinds rest
  End _ -> []

-- | An enclosing block: one opened by indentation, with the column its
-- items start in, or one in explicit braces.
data Ctx = Implicit Int | Explicit

-- | The layout rule of the Haskell report. Takes the lexemes and the
-- position of the end of the input, which is where the braces closing
-- whatever is still open at the end are put.
layout :: [Lexeme] -> Pos -> Stream
layout ls end = case ls of
  l : _ | kindOf l `elem` [KwModule, LBrace] -> go 0 [] ls
  _ -> open [] ls
  where
    kindOf = tokenKind . lexemeToken
    virtual k = Lexeme [] (Token k Text.empty)
    plain l rest = Next l rest Nothing
    line = posLine . lexemePos

    -- Start the block that follows a layout keyword.
    open ctx [] = plain (virtual VLBrace end) (plain (virtual VRBrace end) (go 0 ctx []))
    open ctx rest@(l : _)
      | kindOf l == LBrace = go (line l) ctx rest
      | col > enclosing = plain (virtual VLBrace p) (go (line l) (Implicit col : ctx) rest)
      | otherwise = plain (virtual VLBrace p) (plain (virtual VRBrace p) (go (line l - 1) ctx rest))
      where
        p = lexemePos l
        col = posCol p
        enclosing = case ctx of Implicit m : _ -> m; _ -> 0

    -- @go prev ctx ls@: @prev@ is the line of the previous token, so a token
    -- on a later line is the first on its line.
    go _ ctx [] = foldr (\_ -> plain (virtual VRBrace end)) (End end) (takeWhile implicit ctx)
      where
        implicit = \case Implicit _ -> True; Explicit -> False
    go prev ctx (l : rest)
      | ln > prev, Implicit m : ctx' <- ctx, c < m = plain (virtual VRBrace p) (go prev ctx' (l : rest))
      | ln > prev, Implicit m : _ <- ctx, c == m = plain (virtual VSemicolon p) token
      | otherwise = token
      where
        k = kindOf l
        p@(Pos ln c) = lexemePos l
        token = Next l after closed
        -- Had the block been closed before this token: the brace, and then
        -- the token again, in the block outside.
        closed = case ctx of
          Implicit _ : ctx' -> Just (plain (virtual VRBrace p) (go ln ctx' (l : rest)))
          _ -> Nothing
        after
          | k `elem` [KwLet, KwWhere, KwDo, KwOf] = open ctx rest
          | k == LBrace = go ln (Explicit : ctx) rest
          | k == RBrace, Explicit : ctx' <- ctx = go ln ctx' rest
          | otherwise = go ln ctx rest
