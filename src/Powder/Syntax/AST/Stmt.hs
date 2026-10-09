{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | The forms of a statement in a @do@ block. Import qualified.
module Powder.Syntax.AST.Stmt
  ( Stmt (Bind, Let, Expr),
  )
where

import Powder.Syntax.AST.Internal hiding (Expr)
import qualified Powder.Syntax.AST.Internal as Internal
import qualified Powder.Syntax.Kind as K
import qualified Powder.Syntax.Red as Red

kindIs :: K.SyntaxKind -> Stmt -> Maybe Red.Red
kindIs k (syntax -> r) = if Red.kind r == k then Just r else Nothing

-- | @pattern <- action@
pattern Bind :: Pat -> Internal.Expr -> Stmt
pattern Bind pat action <- (kindIs K.BindStmt -> Just (bind -> (pat, action)))

bind :: Red.Red -> (Pat, Internal.Expr)
bind r = (child 0 r, child 0 r)

pattern Let :: [Decl] -> Stmt
pattern Let decls <- (kindIs K.LetStmt -> Just (children -> decls))

-- | An action run for its effect, or the block's result.
pattern Expr :: Internal.Expr -> Stmt
pattern Expr action <- (kindIs K.ExprStmt -> Just (child 0 -> action))

{-# COMPLETE Bind, Let, Expr #-}
