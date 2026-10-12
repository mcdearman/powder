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
| M1 | The library hierarchy | waiting |
| M2 | Extensions that are only syntax | in progress |
| M3 | Deriving | not started |
| M4 | Explicit quantification at rank 1 | not started |
| M5 | The class system | not started |
| M6 | Higher ranks and existentials | not started |
| M7 | GADTs, type families and what needs an implication solver | not started |
| M8 | Kinds | not started |

**M0, plumbing.** Done. LANGUAGE pragmas are read, an edition names a set of
extensions, and a package's `Powder.toml` sets both for all its modules. Asking
by name for an extension that is not implemented is an error.

**M1, the library hierarchy.** Applicative above Monad, MonadFail, Semigroup
above Monoid, Foldable and Traversable, and a Prelude that exports them. To
be decided first: one library for every edition, as GHC has, or the Report's
library kept beside it for the Haskell2010 edition.

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

Next, in this order:

- [ ] GADTSyntax
- [ ] ExplicitNamespaces
- [ ] DisambiguateRecordFields
- [ ] StrictData
- [ ] RecordWildCards
- [ ] ViewPatterns, once a pattern can hold an expression
- [ ] BangPatterns on `let` and `where` bindings
- [ ] PatternSynonyms, the simple ones

And after those, in no order yet:

- [ ] LexicalNegation
- [ ] NumDecimals
- [ ] UnicodeSyntax
- [ ] ParallelListComp
- [ ] PackageImports

**M3, deriving.** StandaloneDeriving, DerivingStrategies, DeriveFunctor,
DeriveFoldable, DeriveTraversable, GeneralisedNewtypeDeriving; then
DefaultSignatures and DeriveAnyClass.

**M4, explicit quantification at rank 1.** ExplicitForAll,
ScopedTypeVariables, InstanceSigs, ConstrainedClassMethods, KindSignatures,
StandaloneKindSignatures, TypeOperators, TypeApplications with
AllowAmbiguousTypes, NamedWildCards and PartialTypeSignatures, MonoLocalBinds;
and OverloadedStrings. The checker's metavariables need levels, and a scheme's
binders need an order.

**M5, the class system.** FlexibleContexts, FlexibleInstances,
TypeSynonymInstances, MultiParamTypeClasses, ConstraintKinds; then
FunctionalDependencies, UndecidableInstances and the overlap pragmas. A
predicate comes to hold a list of types and the solver becomes a worklist: the
first redesign of the checker.

**M6, higher ranks and existentials.** RankNTypes, ExistentialQuantification;
QuantifiedConstraints perhaps.

**M7, what needs an implication solver.** GADTs, type equality, TypeFamilies,
DeriveGeneric, Coercible and RoleAnnotations, DerivingVia. An OutsideIn solver:
the second redesign of the checker.

**M8, kinds.** PolyKinds, DataKinds, type-level literals, Typeable and
DeriveDataTypeable. This replaces `src/Powder/Kinds.hs`.

### On the side

- PatternSynonyms beyond the simple ones, after M2. Powder's own source has 53
  that go one way.
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

- What Powder2026, the default edition, turns on, and what extensions of
  Powder's own it has. Today it is the thirteen extensions of M2 that are done.
- Whether the register machine keeps tagged values with reference counts, as it
  does on its branch, or takes untagged words and a tracing collector.
- Whether `examples/deep.hs` keeps its million, and runs on the Rust machine
  only, or is made smaller.
