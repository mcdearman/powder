{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | The forms of a type. Import qualified.
module Powder.Syntax.AST.Type
  ( Type (Var, Con, App, Fun, Tuple, List, Paren, Qualified, Forall, Wild, Kinded, Operators, PromotedList, Lit),
    forallKinds,
  )
where

import qualified Data.Text as Text
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

-- | @forall a b. type@: the variables, and the type they are in.
pattern Forall :: [Name] -> Type -> Type
pattern Forall variables ty <- (kindIs K.TyForall -> Just (forall' -> (variables, ty)))

forall' :: Red.Red -> ([Name], Type)
forall' r = (drop 1 (concat [if Red.kind c == K.VarId then [nameOf' c] else [nameOf c | Red.kind c == K.KindedBinder] | c <- Red.children r]), child 0 r)
  where
    nameOf' = Text.unpack . Red.text

-- | The kinds a forall gives the variables it names, for those it does.
forallKinds :: Type -> [(Name, Kind)]
forallKinds = kindedBinders . syntax

-- | @(type :: kind)@
pattern Kinded :: Type -> Kind -> Type
pattern Kinded ty kind <- (kindIs K.TyKinded -> Just (kinded -> Just (ty, kind)))

kinded :: Red.Red -> Maybe (Type, Kind)
kinded r = (,) (child 0 r) <$> kindOf r

-- | Types with operators between them: the types, and the operators.
pattern Operators :: [Type] -> [Name] -> Type
pattern Operators operands operators <- (kindIs K.TyInfix -> Just (operators' -> (operands, operators)))

operators' :: Red.Red -> ([Type], [Name])
operators' r = (children r, [nameOf o | o <- Red.nodes r, Red.kind o == K.Operator])

-- | @_@: a type left to be found.
pattern Wild :: Type
pattern Wild <- (kindIs K.TyWild -> Just _)

-- | @'[a, b]@: a list of types, as a type.
pattern PromotedList :: [Type] -> Type
pattern PromotedList elements <- (kindIs K.TyPromotedList -> Just (children -> elements))

-- | A number or a string, as a type.
pattern Lit :: Literal -> Type
pattern Lit l <- (kindIs K.TyLit -> Just (literalOf -> l))

{-# COMPLETE Var, Con, App, Fun, Tuple, List, Paren, Qualified, Forall, Wild, Kinded, Operators, PromotedList, Lit #-}
