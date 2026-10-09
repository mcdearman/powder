-- | Red trees: a green tree seen from somewhere in it.
--
-- A red node is a green node together with its offset in the file and its
-- parent. It is made on the way down, when a child is asked for, and costs
-- nothing until then.
module Powder.Syntax.Red
  ( Red,
    root,
    green,
    kind,
    offset,
    width,
    end,
    text,
    parent,
    children,
    nodes,
    tokens,
    isToken,
    ancestors,
    descendants,
    nextSibling,
    prevSibling,
    firstToken,
    tokenAt,
    position,
    range,
  )
where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Powder.Syntax.Green (Green)
import qualified Powder.Syntax.Green as Green
import Powder.Syntax.Kind (SyntaxKind, isTrivia)
import qualified Powder.Syntax.Kind as Kind
import Powder.Syntax.Token (Pos (..))

data Red = Red
  { green :: Green,
    -- | Where it starts, in characters from the start of the file.
    offset :: !Int,
    parent :: Maybe Red,
    -- | Which of its parent's children it is.
    index :: !Int
  }

-- | Two red nodes are the same if they are the same piece of the same tree.
instance Eq Red where
  a == b = offset a == offset b && kind a == kind b && width a == width b && depth a == depth b
    where
      depth = length . ancestors

root :: Green -> Red
root g = Red g 0 Nothing 0

kind :: Red -> SyntaxKind
kind = Green.kind . green

width :: Red -> Int
width = Green.width . green

-- | The offset just past it.
end :: Red -> Int
end r = offset r + width r

text :: Red -> Text
text = Green.text . green

isToken :: Red -> Bool
isToken = Kind.isToken . kind

-- | Everything directly under a node: nodes and tokens, trivia included.
children :: Red -> [Red]
children r = zipWith3 child [0 ..] offsets greens
  where
    greens = Green.children (green r)
    offsets = scanl (+) (offset r) (map Green.width greens)
    child i o g = Red g o (Just r) i

-- | The nodes directly under a node.
nodes :: Red -> [Red]
nodes = filter (not . isToken) . children

-- | The tokens directly under a node, leaving out trivia and the layout
-- pass's additions.
tokens :: Red -> [Red]
tokens = filter (\c -> isToken c && not (isTrivia (kind c)) && not (Kind.isVirtual (kind c))) . children

-- | The node itself, then its parent, and so on up to the root.
ancestors :: Red -> [Red]
ancestors r = r : maybe [] ancestors (parent r)

-- | The node itself and everything under it, in the order of the file.
descendants :: Red -> [Red]
descendants r = r : concatMap descendants (children r)

nextSibling, prevSibling :: Red -> Maybe Red
nextSibling r = parent r >>= listToMaybe . drop (index r + 1) . children
prevSibling r
  | index r == 0 = Nothing
  | otherwise = parent r >>= listToMaybe . drop (index r - 1) . children

-- | The first token under a node that is not trivia.
firstToken :: Red -> Maybe Red
firstToken r = listToMaybe [t | t <- descendants r, isToken t, not (isTrivia (kind t)), not (Kind.isVirtual (kind t))]

-- | The token that covers an offset.
tokenAt :: Int -> Red -> Maybe Red
tokenAt o r
  | o < offset r || o >= end r = Nothing
  | isToken r = Just r
  | otherwise = listToMaybe [t | c <- children r, Just t <- [tokenAt o c]]

-- | The line and column a node starts at.
position :: Red -> Pos
position r = positionOf (offset r) r

-- | Where the text of a node starts, leaving out any trivia before it,
-- and where it ends.
range :: Red -> (Pos, Pos)
range r = (maybe (position r) position (firstToken r), positionOf (end r) r)

-- | The line and column of an offset into the tree a node is in.
positionOf :: Int -> Red -> Pos
positionOf o r = Text.foldl' step (Pos 1 1) (Text.take o (text (last (ancestors r))))
  where
    step (Pos l _) '\n' = Pos (l + 1) 1
    step (Pos l c) _ = Pos l (c + 1)
