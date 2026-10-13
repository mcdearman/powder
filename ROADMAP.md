# Roadmap

Where Powder is going, and how far it has got. Powder began as a compiler for
Haskell 2010 and passes its conformance suite for the Report; the aim from here
is the language people write today, which means GHC2024 and a handful of
extensions beside it, behind [editions](README.md#editions-and-extensions).

A milestone's state is changed in the commit that changes it.

| State | Meaning |
|---|---|
| done | merged to `main` |
| in progress | some of it is merged, or a branch is open |
| waiting | needs a decision before it can start |
| not started | |

## The language

### What GHC2024 is

What Haskell 2010 mode has already: DoAndIfThenElse, EmptyDataDecls,
FieldSelectors, ForeignFunctionInterface, ImplicitPrelude,
MonomorphismRestriction, PatternGuards, RelaxedPolyRec, StarIsType,
TraditionalRecordSyntax.

GHC2021's 36: BangPatterns, BinaryLiterals, ConstrainedClassMethods,
ConstraintKinds, DeriveDataTypeable, DeriveFoldable, DeriveFunctor,
DeriveGeneric, DeriveLift, DeriveTraversable, EmptyCase, EmptyDataDeriving,
ExistentialQuantification, ExplicitForAll, FlexibleContexts, FlexibleInstances,
GADTSyntax, GeneralisedNewtypeDeriving, HexFloatLiterals, ImportQualifiedPost,
InstanceSigs, KindSignatures, MultiParamTypeClasses, NamedFieldPuns,
NamedWildCards, NumericUnderscores, PolyKinds, PostfixOperators, RankNTypes,
ScopedTypeVariables, StandaloneDeriving, StandaloneKindSignatures,
TupleSections, TypeApplications, TypeOperators, TypeSynonymInstances.

GHC2024's 8 more: DataKinds, DerivingStrategies, DisambiguateRecordFields,
ExplicitNamespaces, GADTs, LambdaCase, MonoLocalBinds, RoleAnnotations.

Off in GHC2021 and GHC2024 though on in Haskell 2010 mode: DatatypeContexts,
CUSKs, DeepSubsumption. MonoLocalBinds and PolyKinds change what some Haskell
2010 programs mean.

### Milestones

| | Milestone | State |
|---|---|---|
| M0 | Plumbing: LANGUAGE pragmas, editions, defaults in `Powder.toml` | done |
| M1 | The library hierarchy | done |
| M2 | Extensions that are only syntax | done |
| M3 | Deriving | done |
| M4 | Explicit quantification at rank 1 | done |
| M5 | The class system | done |
| M6 | Higher ranks and existentials | done |
| M7 | GADTs, type families and what needs an implication solver | in progress |
| M8 | Kinds | in progress |

**M0, plumbing.** Done. LANGUAGE pragmas are read, an edition names a set of
extensions, and a package's `Powder.toml` sets both for all its modules. Asking
by name for an extension that is not implemented is an error.

**M1, the library hierarchy.** There is one library for every edition, as
GHC has. An edition chooses only which of its names the Prelude has: the
Report's and no others in Haskell2010, by the extension `ReportPrelude`, and
what GHC's Prelude has in the rest.

- [x] Applicative above Monad, with Alternative, and MonadFail
- [x] Semigroup above Monoid
- [x] Foldable and Traversable, and the Prelude's folds over them
- [x] `Control.Applicative`, `Data.Functor`, `Data.Function`, `Data.Foldable`,
      `Data.Traversable`, `Data.Monoid`, `Data.Semigroup`
- [x] Num without Eq and Show above it, as GHC has it
- [x] What else of `base` programs lean on: `Word` in the Prelude,
      `Data.List.NonEmpty` with `sconcat` and `stimes`, `Data.Functor.Identity`
      and `Data.Functor.Const`, `Data.Either`, `Data.Ord`, `Data.Tuple`,
      `Data.Bool`, `Data.Bifunctor`, `Data.IORef`

Left for the milestones that make them possible: `Control.Exception`, whose
`SomeException` is an existential type over a class with `Typeable` above it
(M6 and M8); `Data.Coerce` (M7); `Data.Kind`, `Data.Proxy` and `Data.Void`'s
`absurd` over an empty case are small and come with M4.

**M2, extensions that are only syntax.**

- [x] LambdaCase
- [x] TupleSections
- [x] EmptyCase
- [x] MultiWayIf
- [x] BlockArguments
- [x] ImportQualifiedPost
- [x] NumericUnderscores
- [x] BinaryLiterals
- [x] HexFloatLiterals
- [x] NamedFieldPuns
- [x] BangPatterns, in patterns
- [x] PostfixOperators
- [x] EmptyDataDeriving

- [x] GADTSyntax
- [x] ExplicitNamespaces
- [x] DisambiguateRecordFields
- [x] StrictData
- [x] RecordWildCards
- [x] ViewPatterns
- [x] BangPatterns on `let` and `where` bindings
- [x] PatternSynonyms
- [x] LexicalNegation
- [x] NumDecimals
- [x] UnicodeSyntax
- [x] ParallelListComp
- [x] PackageImports

What these do not yet do, each of which is refused where it is met:

- A view pattern or a pattern synonym in a pattern that is bound, as in
  `let (f -> x) = e`: only where patterns are matched, in a function's
  arguments, a lambda's, a case, a `do` or a comprehension.
- A pattern synonym with fields, one bundled with a type in an export list,
  and an equation written infix after a synonym's `where`. A synonym's type
  signature is read and not checked: its type is what its pattern has.
- `..` in a pattern bound at the top of a module.
- `forall` and the star written in Unicode, which wait for M4.

**M3, deriving.**

- [x] DeriveFunctor, DeriveFoldable, DeriveTraversable
- [x] StandaloneDeriving
- [x] DerivingStrategies: `stock`, `newtype` and `anyclass`
- [x] GeneralisedNewtypeDeriving
- [x] DeriveAnyClass

What these do not yet do:

- An instance derived apart from a type is for a type its own module
  declares. The context it gives is used for a newtype's instance, and for
  any other the context is worked out as it is beside the type.
- A newtype's instance asks of the newtype's parameters what the instance it
  is taken from asks, which is found from the instances declared above it.
  For one declared below, in the same module, it asks the class of each
  parameter, which may be more than is needed.
- DefaultSignatures goes with DeriveGeneric in M7, which is what it is for.

**M4, explicit quantification at rank 1.**

- [x] ExplicitForAll
- [x] ScopedTypeVariables, with types on patterns
- [x] InstanceSigs
- [x] ConstrainedClassMethods
- [x] KindSignatures and StandaloneKindSignatures
- [x] TypeOperators
- [x] TypeApplications, with AllowAmbiguousTypes
- [x] PartialTypeSignatures and NamedWildCards
- [x] OverloadedStrings

None of it needed levels on the checker's metavariables: a signature's type
variables are rigid and known by their identity, so one in scope further in
is the same variable there, and a scheme's variables were in order already.

What these do not yet do:

- A type on a pattern constrains what the pattern matches, and cannot bring
  a type variable into scope: one it mentions that is not in scope is its
  own. It is where a view pattern may be, and not in a pattern that is bound.
- A type with parts left out has all its variables left out with them: what
  is found for it has that shape, and may be less general than its variables
  say. What it leaves out is not reported.
- An instance's own signature for a method is read and not checked against
  the class's.
- A class or an instance is not declared with an operator between its
  types, which waits for classes of more than one parameter in M5.
- MonoLocalBinds goes with M7, which is what needs it.

**M5, the class system.** The first redesign of the checker: a class is
asked of as many types as it has variables, and an instance is found by
fitting its types to those wanted, not by the one type constructor it is
for.

- [x] MultiParamTypeClasses, classes of no types among them
- [x] FunctionalDependencies
- [x] FlexibleInstances and TypeSynonymInstances
- [x] FlexibleContexts
- [x] The overlap pragmas: OVERLAPPING, OVERLAPPABLE, OVERLAPS
- [x] ConstraintKinds
- [x] UndecidableInstances

What these do not yet do:

- No instance is checked to be one whose use must end: UndecidableInstances
  is as good as always on, and the search for an instance has no depth at
  which it gives up.
- A class's dependencies are used to find types, and instances are not
  checked to agree with them.
- A name given to constraints is known in the package that gives it, and
  not in another compiled apart. `Constraint` as a kind is read as `Type`.
- INCOHERENT is not read.
- A newtype's instance of a class of several types is not derived.

**M6, higher ranks and existentials.**

- [x] ExistentialQuantification
- [x] RankNTypes

What these do not yet do:

- A type variable a constructor has of its own is not checked to stay
  where it is matched: a program that lets one out is not refused, and is
  wrong.
- A constructor with a context, or type variables of its own, does not have
  its fields named, has no instances derived for it, and is not matched in a
  pattern that is bound.
- A type for all types is in an argument's place, a field's, or a newtype's:
  not after an arrow, and not what a type variable is. Two are the same only
  if they were written once.
- QuantifiedConstraints is not done.

**M7, what needs an implication solver.**

- [x] GADTs
- [x] Type equality, `a ~ b`, in a context
- [x] RoleAnnotations, which are read and say nothing
- [x] MonoLocalBinds, which is accepted and changes nothing
- [x] Coercible, with `Data.Coerce`
- [x] DerivingVia
- [x] TypeFamilies: functions from types to a type, open, with all their
      equations, or going with a class
- [ ] DeriveGeneric, with `GHC.Generics`. It needs the names of types and
      constructors as types, which is M8's
- [ ] DefaultSignatures, which is for the same

GADTs are done without the implication solver the plan called for. Matching
a constructor that makes its type at types of its own choosing says what
type variables are, and that is kept beside what is given: types are read
through it wherever two are made the same, and what is wanted under the
match is seen to before leaving it. So what is under such a match must have
its types known from outside, by a signature, which is also what GHC asks.

What these do not yet do:

- A local definition under such a match is generalised as anywhere else.
  MonoLocalBinds would not have it be, and a program that leans on the
  difference may be accepted here and refused by GHC.
- Nothing checks that a type variable such a match learns about stays under
  the match, as nothing does for a constructor's own.
- A constructor that makes its type at types of its own choosing does not
  have its fields named, and has no instances derived.
- That two types are the same can be asked for and given in a context. It is
  a class in the library, with one instance, and a dictionary that holds
  nothing is passed for it.
- Coercible takes every parameter of a type to be one whose values may be
  taken as another's: there are no roles, so `coerce` between two sets of
  different orderings is not refused. A newtype is seen through whether or
  not its constructor is in scope.
- An instance is derived by way of another type beside the type's
  declaration, and not apart from it; and not for a class of type
  constructors.
- A function from types is worked out wherever two types are made the same
  and wherever something is wanted of a type. Where what it is applied to is
  not known yet, whether two types are the same is asked again later. That
  one is some type cannot be given in a context: `(F a ~ Int) =>` is read
  and not used.
- There are no data families, no defaults for a type that goes with a class,
  and nothing says a function from types is one to one.
- A function from types and its equations are known in the package that
  declares them, and not in another compiled apart.
- Equations given apart are not checked against each other.

**M8, kinds.**

- [x] DataKinds: a data type as a kind, its constructors as types, and
      lists of types
- [x] PolyKinds, where a kind is written with a variable in it
- [ ] Type-level literals, with `GHC.TypeLits`
- [ ] Typeable, with `Data.Typeable`, and DeriveDataTypeable

`src/Powder/Kinds.hs` was to be replaced. It was added to: a kind may now be
a data type applied to kinds, and a kind written with a variable in it is
read anew, as kinds not yet known, at each use of what was said to have it.

What these do not yet do:

- A kind is general only where it is written with a variable. Nothing
  generalises the kind that is found for a declaration: `data Proxy a =
  Proxy` without the `(a :: k)` has a parameter that is a type, and not one
  of any kind, as GHC would have it with PolyKinds on.
- A kind variable is a kind not yet known each time it is read, so nothing
  holds two written `k`s in one declaration to be the same kind at a use.
  A program GHC would refuse for that is accepted.
- The body of a declaration whose parameter is of a general kind is not
  checked against that kind.
- There is no `forall k.` in a kind or a type, no kind applied by name
  other than a list's, and a type synonym, a class or a function from types
  is not a kind or a type by promotion: only a data type and its
  constructors are.
- A tuple of types, `'(a, b)`, is not a type.
- A kind said of a type or of a parameter is known in the package that
  says it, and not in another compiled apart.

### On the side

- The rest of PatternSynonyms: those with fields, and those bundled with a
  type.
- CPP, before any package from elsewhere is compiled.
- Not planned: Template Haskell (and so DeriveLift), LinearTypes, Arrows,
  ImplicitParams (HasCallStack is to be a constraint that is always solved),
  and MagicHash in earnest.

A typed Core is not needed by anything in GHC2024.

### Compiling Powder with Powder

Powder cannot build itself yet. What stands in the way is small: M0 to M2,
simple PatternSynonyms, signatures on patterns, StandaloneDeriving, and a
decision about Binary and Generic. The packages it depends on (bytestring,
text, binary, diagnose) are better replaced by modules of its own than
compiled.

## The machine

| | State |
|---|---|
| The sequent calculus backend: Core to λμμ̃ to AxCut to bytecode, run in Rust | done |
| Calling C, and being called back by it | done |
| A collector for cycles, beside the reference counts | done |
| The machine two to three times faster | done |
| A register machine with its own stack of frames, not the native one | in progress |
| Stack segments that can be detached, for effects and continuations | not started |
| A tracing collector, as an option | not started |
| Compiling to native code, ahead of time or as the program runs | not started |

**The register machine** is on the branch `register-vm`. It passes the compiler's
suite and runs about twice as fast as the machine on `main`. Left to do before
it can merge: `examples/deep.hs`, a recursion a million deep, gives the right
answer but takes 17 seconds, and why is not yet known; the build tool's suite
has not been run on it; and the README still describes the old machine.

## The tools

| | State |
|---|---|
| A build tool, with packages compiled apart | done |
| The Haskell 2010 conformance suite, passing in full | done |
| The suite run on every pull request | done |

## To be decided

- Whether `ReportPrelude` is the name to keep for the extension that says a
  module has the Report's Prelude.
- What Powder2026, the default edition, turns on, and what extensions of
  Powder's own it has. Today it is the thirteen extensions of M2 that are done.
- Whether the register machine keeps tagged values with reference counts, as it
  does on its branch, or takes untagged words and a tracing collector.
- Whether `examples/deep.hs` keeps its million, and runs on the Rust machine
  only, or is made smaller.
