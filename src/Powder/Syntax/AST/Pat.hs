{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | The forms of a pattern. Import qualified.
module Powder.Syntax.AST.Pat
  ( Pat (Var, Wild, Lit, NegLit, Con, Infix, As, Paren, Tuple, List, Lazy, Record, Bang, View),
    recordWild,
  )
where

import Powder.Syntax.AST.Internal
import qualified Powder.Syntax.Kind as K
import qualified Powder.Syntax.Red as Red

kindIs :: K.SyntaxKind -> Pat -> Maybe Red.Red
kindIs k (syntax -> r) = if Red.kind r == k then Just r else Nothing

pattern Var :: Name -> Pat
pattern Var name <- (kindIs K.VarPat -> Just (nameOf -> name))

pattern Wild :: Pat
pattern Wild <- (kindIs K.WildPat -> Just _)

pattern Lit :: Literal -> Pat
pattern Lit literal <- (kindIs K.LitPat -> Just (literalOf -> literal))

-- | A negative number: the literal here is the number after the minus.
pattern NegLit :: Literal -> Pat
pattern NegLit literal <- (kindIs K.NegLitPat -> Just (literalOf -> literal))

-- | A constructor and the patterns of its fields.
pattern Con :: Name -> [Pat] -> Pat
pattern Con name fields <- (kindIs K.ConPat -> Just (con -> (name, fields)))

con :: Red.Red -> (Name, [Pat])
con r = (constructorNameOf r, children r)

-- | Two patterns with a constructor written between them: @head : tail@.
pattern Infix :: Pat -> Name -> Pat -> Pat
pattern Infix left name right <- (kindIs K.InfixPat -> Just (infixPat -> Just (left, name, right)))

infixPat :: Red.Red -> Maybe (Pat, Name, Pat)
infixPat r = case children r of
  [left, right] -> Just (left, nameOf r, right)
  _ -> Nothing

-- | A constructor and patterns for the fields named.
pattern Record :: Name -> [(Name, Maybe Pat)] -> Pat
pattern Record name fields <- (kindIs K.RecordPat -> Just (recordPat -> (name, fields)))

-- | Whether a pattern with fields ends in @..@: the fields not named are
-- each bound to the variable of its name.
recordWild :: Pat -> Bool
recordWild = fieldWild . syntax

recordPat :: Red.Red -> (Name, [(Name, Maybe Pat)])
recordPat r = (nameOf r, fieldBinds r)

-- | @name\@pattern@
pattern As :: Name -> Pat -> Pat
pattern As name inner <- (kindIs K.AsPat -> Just (as -> (name, inner)))

as :: Red.Red -> (Name, Pat)
as r = (nameOf r, child 0 r)

pattern Paren :: Pat -> Pat
pattern Paren inner <- (kindIs K.ParenPat -> Just (child 0 -> inner))

-- | A tuple, or with no components the unit.
pattern Tuple :: [Pat] -> Pat
pattern Tuple components <- (kindIs K.TuplePat -> Just (children -> components))

pattern List :: [Pat] -> Pat
pattern List elements <- (kindIs K.ListPat -> Just (children -> elements))

-- | A pattern that always matches, and is taken apart only when one of
-- its variables is wanted.
pattern Lazy :: Pat -> Pat
pattern Lazy inner <- (kindIs K.LazyPat -> Just (child 0 -> inner))

-- | @!pattern@
pattern Bang :: Pat -> Pat
pattern Bang inner <- (kindIs K.BangPat -> Just (child 0 -> inner))

-- | @function -> pattern@
pattern View :: Expr -> Pat -> Pat
pattern View function inner <- (kindIs K.ViewPat -> Just (view -> (function, inner)))

view :: Red.Red -> (Expr, Pat)
view r = (child 0 r, child 0 r)

{-# COMPLETE Var, Wild, Lit, NegLit, Con, Infix, As, Paren, Tuple, List, Lazy, Record, Bang, View #-}
