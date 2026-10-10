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
  | -- | Fields of a record constructor that share a type: @a, b :: T@.
    FieldDecl
  | -- | @field = value@, in a record expression or pattern.
    FieldBind
  | Deriving
  | TypeDecl
  | -- | A class: its head, a type, and the declarations in its @where@.
    ClassDecl
  | InstanceDecl
  | ImportDecl
  | -- | @default (types)@
    DefaultDecl
  | -- | @foreign import ccall "entity" name :: type@
    ForeignDecl
  | -- | @foreign export ccall "entity" name :: type@
    ForeignExportDecl
  | TypeSig
  | FixityDecl
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
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Whether a kind is a token's rather than a node's.
isToken :: SyntaxKind -> Bool
isToken k = k <= Error

-- | Whitespace, comments and pragmas: tokens the parser passes over.
isTrivia :: SyntaxKind -> Bool
isTrivia k = k `elem` [Whitespace, LineComment, BlockComment, Pragma]

isVirtual :: SyntaxKind -> Bool
isVirtual k = k `elem` [VLBrace, VRBrace, VSemicolon]
