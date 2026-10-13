-- | The kinds of token and of tree node. One type for both, so that a tree
-- is homogeneous: every node and leaf is told apart only by its kind.
module Powder.Syntax.Kind
  ( SyntaxKind (..),
    isTrivia,
    isVirtual,
    isToken,
  )
where

data SyntaxKind
  = -- Tokens that carry no meaning.
    Whitespace
  | LineComment
  | BlockComment
  | -- | @{-# ... #-}@
    Pragma
  | -- Names.
    VarId
  | ConId
  | QVarId
  | QConId
  | VarSym
  | ConSym
  | QVarSym
  | QConSym
  | -- | A minus with something right after it and nothing right before:
    -- with LexicalNegation, the sign of what follows.
    PrefixMinus
  | -- Literals.
    IntegerLit
  | FloatLit
  | CharLit
  | StringLit
  | -- Reserved words.
    KwCase
  | KwClass
  | KwData
  | KwDefault
  | KwDeriving
  | KwDo
  | KwElse
  | KwForeign
  | KwIf
  | KwImport
  | KwIn
  | KwInfix
  | KwInfixl
  | KwInfixr
  | KwInstance
  | KwLet
  | KwModule
  | KwNewtype
  | KwOf
  | KwThen
  | KwType
  | KwWhere
  | Underscore
  | -- Reserved operators.
    DotDot
  | Colon
  | DoubleColon
  | Equals
  | Backslash
  | Bar
  | LeftArrow
  | RightArrow
  | At
  | -- | @~@ just before a pattern, which makes it lazy. Anywhere else a
    -- @~@ is an operator.
    Tilde
  | -- | @!@ just before a type, which makes a field strict. Anywhere else a
    -- @!@ is an operator.
    Bang
  | DoubleArrow
  | -- Punctuation.
    LParen
  | RParen
  | Comma
  | Semicolon
  | LBracket
  | RBracket
  | Backquote
  | LBrace
  | RBrace
  | -- Braces and semicolons the layout pass adds. They have no text.
    VLBrace
  | VRBrace
  | VSemicolon
  | -- | The end of the input, for the parser to look ahead to. Never in a
    -- tree.
    Eof
  | -- | Text that is no token: a stray character, or a literal or comment
    -- that does not end.
    Error
  | -- Nodes: the roots.
    Module
  | -- | Where the parser found something it could not use, or nothing where
    -- it needed something.
    ErrorNode
  | -- | An expression entered on its own, at a prompt.
    ReplExpr
  | ModuleHeader
  | -- Declarations.
    DataDecl
  | ConDecl
  | -- | The type a constructor makes, where the constructor is given by
    -- its type: what comes last in @C :: a -> T a@.
    GadtResult
  | -- | A context before a constructor: what is asked of its types where
    -- it is applied, and known of them where it is matched.
    ConContext
  | -- | @forall a b.@ before a constructor: type variables of its own.
    ConForall
  | -- | @..@ among the fields of a record: all the fields not named.
    FieldWild
  | -- | Fields of a record constructor that share a type: @a, b :: T@.
    FieldDecl
  | -- | @field = value@, in a record expression or pattern.
    FieldBind
  | Deriving
  | -- | @deriving instance Class (Type a)@: an instance derived apart from
    -- the type's declaration.
    StandaloneDerivingDecl
  | TypeDecl
  | -- | @type T :: kind@
    KindSigDecl
  | -- | @type role T nominal representational@
    RoleDecl
  | -- | @type family F a@, with perhaps a kind and perhaps equations
    -- after a where; or in a class, @type F a@.
    TypeFamilyDecl
  | -- | @type instance F Int = Bool@; or in an instance, @type F Int = Bool@.
    TypeInstanceDecl
  | -- | One equation of a type family that has them all: @F Int = Bool@.
    TypeEquation
  | -- | A class: its head, a type, and the declarations in its @where@.
    ClassDecl
  | InstanceDecl
  | -- | @| a -> b, c -> d@ after a class's head: which of its variables
    -- say what others are.
    FunDeps
  | ImportDecl
  | -- | @default (types)@
    DefaultDecl
  | -- | @foreign import ccall "entity" name :: type@
    ForeignDecl
  | -- | @foreign export ccall "entity" name :: type@
    ForeignExportDecl
  | TypeSig
  | -- | @default m :: type@, in a class: the type of a method's default.
    DefaultSig
  | FixityDecl
  | -- | @pattern P x = ...@ or @pattern P x <- ...@: a name for a pattern.
    PatSynDecl
  | -- | @pattern P :: type@
    PatSynSig
  | FunClause
  | InfixClause
  | PatBind
  | -- | The name a clause or signature is for.
    Binder
  | Rhs
  | GuardedRhs
  | WhereClause
  | -- Types.
    TyVar
  | -- | A type constructor: a name, or @[]@ for the list's.
    TyCon
  | TyApp
  | TyFun
  | -- | A tuple type, or with no components the unit's.
    TyTuple
  | TyList
  | TyParen
  | -- | @context => type@
    TyQual
  | -- | @forall a b. type@
    TyForall
  | -- | @_@ in a type: whatever type it turns out to be.
    TyWild
  | -- | @(type :: kind)@
    TyKinded
  | -- | Types with operators between them: @a :+: b@.
    TyInfix
  | -- | @'[a, b]@: a list of types, as a type.
    TyPromotedList
  | -- | A number or a string, as a type.
    TyLit
  | -- | A kind, as its tokens.
    KindSig
  | -- | @(a :: kind)@, where a type variable is named.
    KindedBinder
  | -- Expressions.
    VarExpr
  | ConExpr
  | LitExpr
  | AppExpr
  | -- | Operands and operators in a row, as written. Which operator binds
    -- tighter is decided later, once fixities are known.
    InfixExpr
  | -- | A prefix minus in an 'InfixExpr'.
    Negation
  | Operator
  | LamExpr
  | -- | @\\case alternatives@
    LamCaseExpr
  | -- | @if | guard -> e ...@
    MultiIfExpr
  | -- | A tuple with places left out: @(,x)@, and a place left out.
    TupleSection
  | TupleHole
  | LetExpr
  | IfExpr
  | CaseExpr
  | DoExpr
  | ParenExpr
  | TupleExpr
  | ListExpr
  | RangeExpr
  | -- | A range with a second element: @[a, b ..]@ or @[a, b .. c]@.
    RangeStepExpr
  | -- | @[e | qualifiers]@
    ListComp
  | -- | The qualifiers after a second bar or a later one, in a list
    -- comprehension whose qualifiers are gone through side by side.
    ListCompBranch
  | -- | @\@type@ among a function's arguments: the type one of its type
    -- variables is.
    TypeArg
  | SectionL
  | SectionR
  | -- | @expression :: type@
    TypedExpr
  | -- | A record built or updated: an expression and bindings in braces.
    RecordExpr
  | Alt
  | -- Statements.
    BindStmt
  | LetStmt
  | ExprStmt
  | -- Patterns.
    VarPat
  | WildPat
  | LitPat
  | NegLitPat
  | ConPat
  | InfixPat
  | AsPat
  | ParenPat
  | TuplePat
  | ListPat
  | -- | @~pattern@
    LazyPat
  | -- | @!pattern@: what is matched is worked out first.
    BangPat
  | -- | A constructor and patterns for some of its fields, in braces.
    RecordPat
  | -- | @function -> pattern@: matches what the pattern does, of the
    -- function applied to what is matched.
    ViewPat
  | -- | @pattern :: type@
    SigPat
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Whether a kind is a token's rather than a node's.
isToken :: SyntaxKind -> Bool
isToken k = k <= Error

-- | Whitespace, comments and pragmas: tokens the parser passes over.
isTrivia :: SyntaxKind -> Bool
isTrivia k = k `elem` [Whitespace, LineComment, BlockComment, Pragma]

isVirtual :: SyntaxKind -> Bool
isVirtual k = k `elem` [VLBrace, VRBrace, VSemicolon]
