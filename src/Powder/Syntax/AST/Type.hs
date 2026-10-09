{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | The forms of a type. Import qualified.
module Powder.Syntax.AST.Type
  ( Type (Var, Con, App, Fun, Tuple, List, Paren, Qualified),
  )
where

import Powder.Syntax.AST.Internal
import qualified Powder.Syntax.Kind as K
import qualified Powder.Syntax.Red as Red

kindIs :: K.SyntaxKind -> Type -> Maybe Red.Red
kindIs k (syntax -> r) = if Red.kind r == k then Just r else Nothing

pattern Var :: Name -> Type
pattern Var name <- (kindIs K.TyVar -> Just (nameOf -> name))

-- | A type constructor. The list's, written @[]@, is named that.
pattern Con :: Name -> Type
pattern Con name <- (kindIs K.TyCon -> Just (constructorName -> name))

constructorName :: Red.Red -> Name
constructorName = constructorNameOf

-- | A type and the types it is applied to, of which there is at least one.
pattern App :: Type -> [Type] -> Type
pattern App function arguments <- (kindIs K.TyApp -> Just (children -> function : arguments))

pattern Fun :: Type -> Type -> Type
pattern Fun argument result <- (kindIs K.TyFun -> Just (children -> [argument, result]))

-- | A tuple type, or with no components the unit type.
pattern Tuple :: [Type] -> Type
pattern Tuple components <- (kindIs K.TyTuple -> Just (children -> components))

pattern List :: Type -> Type
pattern List element <- (kindIs K.TyList -> Just (child 0 -> element))

pattern Paren :: Type -> Type
pattern Paren inner <- (kindIs K.TyParen -> Just (child 0 -> inner))

-- | @context => type@
pattern Qualified :: Type -> Type -> Type
pattern Qualified context ty <- (kindIs K.TyQual -> Just (children -> [context, ty]))

{-# COMPLETE Var, Con, App, Fun, Tuple, List, Paren, Qualified #-}
