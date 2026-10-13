{-# LANGUAGE TypeOperators, KindSignatures, DataKinds, PolyKinds, TypeFamilies, FlexibleInstances, FlexibleContexts #-}
-- A type's values kept as values of a few types that all types' values
-- can be kept as: so that a function written for those few is one for
-- every type that is an instance of Generic, which a type is by deriving
-- it.
module GHC.Generics where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal)

-- A type with no constructors.
data V1 p

-- A constructor with no fields.
data U1 p = U1 deriving (Eq, Ord, Show)

-- A field.
newtype K1 i c p = K1 c deriving (Eq, Ord, Show)

unK1 :: K1 i c p -> c
unK1 (K1 c) = c

-- What says of a type, a constructor or a field its name and the like.
newtype M1 i (c :: Meta) f p = M1 (f p)

unM1 :: M1 i c f p -> f p
unM1 (M1 x) = x

-- One constructor or another.
data (f :+: g) p = L1 (f p) | R1 (g p)

-- One field and another.
data (f :*: g) p = f p :*: g p

infixr 5 :+:

infixr 6 :*:

data R

data D

data C

data S

type Rec0 = K1 R

type D1 = M1 D

type C1 = M1 C

type S1 = M1 S

-- What is said of a type, of a constructor and of a field, as types.
data Meta
  = MetaData Symbol Symbol Symbol Bool
  | MetaCons Symbol FixityI Bool
  | MetaSel (Maybe Symbol) SourceUnpackedness SourceStrictness DecidedStrictness

data FixityI = PrefixI | InfixI Associativity Nat

data Fixity = Prefix | Infix Associativity Int deriving (Eq, Ord, Show, Read)

data Associativity = LeftAssociative | RightAssociative | NotAssociative
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data SourceUnpackedness = NoSourceUnpackedness | SourceNoUnpack | SourceUnpack
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data SourceStrictness = NoSourceStrictness | SourceLazy | SourceStrict
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data DecidedStrictness = DecidedLazy | DecidedStrict | DecidedUnpack
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- How tightly a constructor binds.
prec :: Fixity -> Int
prec Prefix = 10
prec (Infix _ n) = n

class Generic a where
  type Rep a :: Type -> Type
  from :: a -> Rep a x
  to :: Rep a x -> a

class Datatype (d :: Meta) where
  datatypeName :: t d f a -> String
  moduleName :: t d f a -> String
  packageName :: t d f a -> String
  isNewtype :: t d f a -> Bool

instance (KnownSymbol n, KnownSymbol m, KnownSymbol p, KnownBool nt) => Datatype ('MetaData n m p nt) where
  datatypeName x = symbolVal (dataNameOf x)
  moduleName x = symbolVal (dataModuleOf x)
  packageName x = symbolVal (dataPackageOf x)
  isNewtype x = boolVal (dataNewtypeOf x)

dataNameOf :: t ('MetaData n m p nt) f a -> Proxy n
dataNameOf _ = Proxy

dataModuleOf :: t ('MetaData n m p nt) f a -> Proxy m
dataModuleOf _ = Proxy

dataPackageOf :: t ('MetaData n m p nt) f a -> Proxy p
dataPackageOf _ = Proxy

dataNewtypeOf :: t ('MetaData n m p nt) f a -> Proxy nt
dataNewtypeOf _ = Proxy

class Constructor (c :: Meta) where
  conName :: t c f a -> String
  conFixity :: t c f a -> Fixity
  conIsRecord :: t c f a -> Bool

instance (KnownSymbol n, KnownFixity f, KnownBool r) => Constructor ('MetaCons n f r) where
  conName x = symbolVal (conNameOf x)
  conFixity x = fixityVal (conFixityOf x)
  conIsRecord x = boolVal (conRecordOf x)

conNameOf :: t ('MetaCons n f r) g a -> Proxy n
conNameOf _ = Proxy

conFixityOf :: t ('MetaCons n f r) g a -> Proxy f
conFixityOf _ = Proxy

conRecordOf :: t ('MetaCons n f r) g a -> Proxy r
conRecordOf _ = Proxy

class Selector (s :: Meta) where
  selName :: t s f a -> String

instance KnownMaybeSymbol mn => Selector ('MetaSel mn su ss ds) where
  selName x = maybeSymbolVal (selNameOf x)

selNameOf :: t ('MetaSel mn su ss ds) f a -> Proxy mn
selNameOf _ = Proxy

-- Which of a kind's types a type is, for the kinds what is said is of.
class KnownBool (b :: Bool) where
  boolVal :: Proxy b -> Bool

instance KnownBool 'True where
  boolVal _ = True

instance KnownBool 'False where
  boolVal _ = False

class KnownMaybeSymbol (m :: k) where
  maybeSymbolVal :: Proxy m -> String

instance KnownMaybeSymbol 'Nothing where
  maybeSymbolVal _ = ""

instance KnownSymbol s => KnownMaybeSymbol ('Just s) where
  maybeSymbolVal p = symbolVal (justOf p)

justOf :: Proxy ('Just s) -> Proxy s
justOf _ = Proxy

class KnownAssociativity (a :: Associativity) where
  associativityVal :: Proxy a -> Associativity

instance KnownAssociativity 'LeftAssociative where
  associativityVal _ = LeftAssociative

instance KnownAssociativity 'RightAssociative where
  associativityVal _ = RightAssociative

instance KnownAssociativity 'NotAssociative where
  associativityVal _ = NotAssociative

class KnownFixity (f :: FixityI) where
  fixityVal :: Proxy f -> Fixity

instance KnownFixity 'PrefixI where
  fixityVal _ = Prefix

instance (KnownAssociativity a, KnownNat n) => KnownFixity ('InfixI a n) where
  fixityVal p = Infix (associativityVal (infixAssociativityOf p)) (fromInteger (natVal (infixPrecOf p)))

infixAssociativityOf :: Proxy ('InfixI a n) -> Proxy a
infixAssociativityOf _ = Proxy

infixPrecOf :: Proxy ('InfixI a n) -> Proxy n
infixPrecOf _ = Proxy

-- The types the language and its prelude have, each written out as
-- deriving would have it.
type Con0 n = C1 ('MetaCons n 'PrefixI 'False)

type Sel0 = S1 ('MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)

instance Generic () where
  type Rep () = D1 ('MetaData "Unit" "GHC.Tuple" "ghc-prim" 'False) (Con0 "()" U1)
  from () = M1 (M1 U1)
  to (M1 (M1 U1)) = ()

instance Generic Bool where
  type Rep Bool = D1 ('MetaData "Bool" "GHC.Types" "ghc-prim" 'False) (Con0 "False" U1 :+: Con0 "True" U1)
  from x = M1 (if x then R1 (M1 U1) else L1 (M1 U1))
  to (M1 r) = case r of
    L1 _ -> False
    R1 _ -> True

instance Generic Ordering where
  type Rep Ordering = D1 ('MetaData "Ordering" "GHC.Types" "ghc-prim" 'False) (Con0 "LT" U1 :+: (Con0 "EQ" U1 :+: Con0 "GT" U1))
  from x = M1 (case x of
    LT -> L1 (M1 U1)
    EQ -> R1 (L1 (M1 U1))
    GT -> R1 (R1 (M1 U1)))
  to (M1 r) = case r of
    L1 _ -> LT
    R1 (L1 _) -> EQ
    R1 (R1 _) -> GT

instance Generic (Maybe a) where
  type Rep (Maybe a) = D1 ('MetaData "Maybe" "GHC.Maybe" "base" 'False) (Con0 "Nothing" U1 :+: Con0 "Just" (Sel0 (Rec0 a)))
  from x = M1 (case x of
    Nothing -> L1 (M1 U1)
    Just a -> R1 (M1 (M1 (K1 a))))
  to (M1 r) = case r of
    L1 _ -> Nothing
    R1 (M1 (M1 (K1 a))) -> Just a

instance Generic (Either a b) where
  type Rep (Either a b) = D1 ('MetaData "Either" "Data.Either" "base" 'False) (Con0 "Left" (Sel0 (Rec0 a)) :+: Con0 "Right" (Sel0 (Rec0 b)))
  from x = M1 (case x of
    Left a -> L1 (M1 (M1 (K1 a)))
    Right b -> R1 (M1 (M1 (K1 b))))
  to (M1 r) = case r of
    L1 (M1 (M1 (K1 a))) -> Left a
    R1 (M1 (M1 (K1 b))) -> Right b

instance Generic [a] where
  type Rep [a] = D1 ('MetaData "List" "GHC.Types" "ghc-prim" 'False) (Con0 "[]" U1 :+: C1 ('MetaCons ":" ('InfixI 'RightAssociative 5) 'False) (Sel0 (Rec0 a) :*: Sel0 (Rec0 [a])))
  from x = M1 (case x of
    [] -> L1 (M1 U1)
    a : as -> R1 (M1 (M1 (K1 a) :*: M1 (K1 as))))
  to (M1 r) = case r of
    L1 _ -> []
    R1 (M1 (M1 (K1 a) :*: M1 (K1 as))) -> a : as

instance Generic (a, b) where
  type Rep (a, b) = D1 ('MetaData "Tuple2" "GHC.Tuple" "ghc-prim" 'False) (Con0 "(,)" (Sel0 (Rec0 a) :*: Sel0 (Rec0 b)))
  from (a, b) = M1 (M1 (M1 (K1 a) :*: M1 (K1 b)))
  to (M1 (M1 (M1 (K1 a) :*: M1 (K1 b)))) = (a, b)

instance Generic (a, b, c) where
  type Rep (a, b, c) = D1 ('MetaData "Tuple3" "GHC.Tuple" "ghc-prim" 'False) (Con0 "(,,)" (Sel0 (Rec0 a) :*: (Sel0 (Rec0 b) :*: Sel0 (Rec0 c))))
  from (a, b, c) = M1 (M1 (M1 (K1 a) :*: (M1 (K1 b) :*: M1 (K1 c))))
  to (M1 (M1 (M1 (K1 a) :*: (M1 (K1 b) :*: M1 (K1 c))))) = (a, b, c)
