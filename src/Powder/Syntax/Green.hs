-- | Green trees: the syntax of a file as written, every character of it.
--
-- A green tree is immutable and knows nothing of where it sits: a node is
-- a kind, a width and its children, and a token is a kind and its text. So
-- equal pieces of syntax are equal trees, wherever they are, and can be
-- shared. Position and parents are a 'Powder.Syntax.Red.Red' tree's to
-- supply.
module Powder.Syntax.Green
  ( Green,
    node,
    token,
    kind,
    width,
    children,
    tokenText,
    text,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Builder
import Powder.Syntax.Kind

data Green
  = Node !SyntaxKind !Int [Green]
  | Token !SyntaxKind !Text
  deriving (Eq)

node :: SyntaxKind -> [Green] -> Green
node k cs = Node k (sum (map width cs)) cs

token :: SyntaxKind -> Text -> Green
token = Token

kind :: Green -> SyntaxKind
kind = \case
  Node k _ _ -> k
  Token k _ -> k

-- | How many characters of the file it covers.
width :: Green -> Int
width = \case
  Node _ w _ -> w
  Token _ t -> Text.length t

-- | A node's children; a token has none.
children :: Green -> [Green]
children = \case
  Node _ _ cs -> cs
  Token _ _ -> []

-- | A token's text; nothing for a node.
tokenText :: Green -> Maybe Text
tokenText = \case
  Token _ t -> Just t
  Node {} -> Nothing

-- | The text it was parsed from, exactly.
text :: Green -> Text
text = Lazy.toStrict . Builder.toLazyText . go
  where
    go = \case
      Token _ t -> Builder.fromText t
      Node _ _ cs -> foldMap go cs
