-- | Types as the type checker has them.
module Powder.Types
  ( Type (..),
    Scheme (..),
    Pred (..),
    Evidence (..),
    metas,
    rigids,
    substitute,
    prettyType,
    prettyScheme,
  )
where

import qualified Data.IntMap.Strict as IntMap
import Data.List (intercalate, nub, (\\))
import Powder.HIR (Builtins (..), NameId (..), NameInfo (..), NodeId, Program (..), nameInfo)

-- | A type with no quantifier in it.
data Type
  = -- | A type constructor.
    Con NameId
  | App Type Type
  | Fun Type Type
  | -- | A type variable that stands for itself: one a signature or a data
    -- declaration names, or one that generalising made. It is equal only
    -- to itself.
    Rigid NameId
  | -- | A type not yet known, to be found by unification.
    Meta Int
  | -- | A type for all types its variables could be that are instances of
    -- what its context asks: the type of something that is given a
    -- function to use at more than one type.
    Quantified [NameId] [Pred] Type
  deriving (Eq, Show)

-- | A class asked of types: one for each variable the class has.
data Pred = Pred NameId [Type]
  deriving (Eq, Show)

inPred :: Pred -> [Type]
inPred (Pred _ ts) = ts

-- | A type for all types its variables could be that are instances of
-- what its context asks.
data Scheme = Forall [NameId] [Pred] Type
  deriving (Eq, Show)

-- | Why a type is an instance of a class: where the dictionary of its
-- methods is to be had from.
data Evidence
  = -- | One a definition is given, for its context: the number is the
    -- parameter's.
    Parameter Int
  | -- | An instance, by its node, given what its own context asks for.
    Instance NodeId [Evidence]
  | -- | The superclass at a place among a class's, out of a dictionary of
    -- that class.
    Superclass NameId Int Evidence
  | -- | None was found, which has been reported.
    Missing
  deriving (Eq, Show)

-- | The unknowns in a type, in order, each once.
metas :: Type -> [Int]
metas = nub . go
  where
    go = \case
      Meta m -> [m]
      App f a -> go f ++ go a
      Fun a b -> go a ++ go b
      Quantified _ context t -> concatMap go (concatMap inPred context) ++ go t
      Con _ -> []
      Rigid _ -> []

-- | The type variables in a type, in order, each once.
rigids :: Type -> [NameId]
rigids = nub . go
  where
    go = \case
      Rigid a -> [a]
      App f a -> go f ++ go a
      Fun a b -> go a ++ go b
      Quantified vs context t -> filter (`notElem` vs) (concatMap go (concatMap inPred context) ++ go t)
      Con _ -> []
      Meta _ -> []

-- | Put types in place of type variables.
substitute :: IntMap.IntMap Type -> Type -> Type
substitute s = go
  where
    go = \case
      Rigid a -> IntMap.findWithDefault (Rigid a) (nameKey a) s
      App f a -> App (go f) (go a)
      Fun a b -> Fun (go a) (go b)
      Quantified vs context t ->
        let inner = substitute (foldr (IntMap.delete . nameKey) s vs)
         in Quantified vs [Pred c (map inner ts) | Pred c ts <- context] (inner t)
      t -> t

-- | A type as it would be written. A type variable with no name of its
-- own is given a letter, and an unknown is written @_@.
prettyType :: Program -> Type -> String
prettyType program t = render program (letters program (rigids t)) 0 t

-- | A scheme as a signature would give it, the quantifier left unsaid.
prettyScheme :: Program -> Scheme -> String
prettyScheme program (Forall vs context t) = constraints ++ render program names 0 t
  where
    names = letters program (nub (vs ++ rigids t))
    name v = maybe "?" nameText (nameInfo program v)
    shown = [unwords (name c : map (render program names 2) as) | Pred c as <- nub context]
    constraints = case shown of
      [] -> ""
      [one] -> one ++ " => "
      several -> "(" ++ intercalate ", " several ++ ") => "

-- | Names for the type variables that have none: letters not already used.
letters :: Program -> [NameId] -> IntMap.IntMap String
letters program vs = IntMap.fromList (zip (map nameKey unnamed) (supply \\ taken))
  where
    named = [(v, nameText info) | v <- vs, Just info <- [nameInfo program v]]
    unnamed = vs \\ map fst named
    taken = map snd named
    supply = map pure ['a' .. 'z'] ++ [c : show n | n <- [1 :: Int ..], c <- ['a' .. 'z']]

render :: Program -> IntMap.IntMap String -> Int -> Type -> String
render program generated = go
  where
    builtins = programBuiltins program
    name v = maybe "?" nameText (nameInfo program v)
    parens yes s = if yes then "(" ++ s ++ ")" else s

    -- 0: anywhere; 1: to the left of an arrow; 2: as an argument.
    go :: Int -> Type -> String
    go p t = case spine t [] of
      (Con c, [element]) | c == builtinList builtins -> "[" ++ go 0 element ++ "]"
      (Con c, arguments@(_ : _ : _)) | isTuple (name c) && length (name c) - 1 == length arguments -> "(" ++ intercalate ", " (map (go 0) arguments) ++ ")"
      (Fun a b, []) -> parens (p > 0) (go 1 a ++ " -> " ++ go 0 b)
      (Quantified vs context body, []) ->
        let asked = [unwords (name c : map (go 2) ts) | Pred c ts <- context]
            constraints = case asked of
              [] -> ""
              [one] -> one ++ " => "
              several -> "(" ++ intercalate ", " several ++ ") => "
         in parens (p > 0) ("forall " ++ unwords (map (atom . Rigid) vs) ++ ". " ++ constraints ++ go 0 body)
      (hd, []) -> atom hd
      (hd, arguments) -> parens (p > 1) (unwords (go 2 hd : map (go 2) arguments))

    atom = \case
      Con c -> if name c == "->" then "(->)" else name c
      Rigid a -> maybe (IntMap.findWithDefault "?" (nameKey a) generated) nameText (nameInfo program a)
      Meta _ -> "_"
      t -> go 2 t

    spine t arguments = case t of
      App f a -> spine f (a : arguments)
      _ -> (t, arguments)

    isTuple s = take 2 s == "(,"
