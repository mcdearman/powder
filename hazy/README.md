# Tests from Hazy

These are tests from [Hazy](https://github.com/Superstar64/Hazy), a Haskell compiler by Freddy Angel Cubas
("superstar64"), copied without change from its `test/` directory at commit
`7e9471397777b34209698aa31da7acbc7220ab14`.

**Licence.** Unlike the rest of this repository, everything in this directory is Hazy's and is under the GNU
General Public License, version 3 only: see [LICENSE](LICENSE). Copyright © 2026 Freddy Angel Cubas. Nothing here
is part of Powder or is linked into it; the files are kept beside it as data for its test suite.

## Which tests

Hazy tests more than Haskell 2010: it has extensions of its own and takes several of GHC's as given. Only the tests
that are Haskell 2010 as the Report has it are here.

- `run/NAME/source/Main.hs` is a program, and `run/NAME/result` is what it prints.
- `good/*.hs` are modules that must be accepted.
- `bad/*/*.hs` are modules that must be refused.

Left out, with why:

| Test | Why |
| --- | --- |
| `run/bounded` | its result has Hazy's 32-bit `Int`, and `'\0'` where the Report's `showLitChar` gives `'\NUL'` |
| `run/readshow` | the same `'\0'` |
| `run/by-name`, `levity`, `strict` | import `Hazy.Prelude` |
| `run/derive-enum-bounded`, `derive-eq-ord`, `derive-show` | standalone deriving, `\case` |
| `run/monad` | `Applicative` and `MonadFail`, which are not in the Report's Prelude |
| `run/monoid`, `surrogate` | `Data.Semigroup`, `Data.Text` |
| `run/multiline` | `MultilineStrings` |
| `run/peano` | a kind signature, `\case` |
| `run/triangular`, `good/Guard` | Hazy's `of` guard blocks |
| `good/Levity`, `LocalData`, `NestedClassShare`, `bad/check/EscapeData`, `Levity` | Hazy's own extensions |
| `good/LocalImport`, `Sloppy`, `Unstable`, `bad/resolve/Unstable` | Hazy's rules for imports |
| `good/Promotion`, `RecursiveType`, `TypeAnnotation`, `bad/check/WrongKind` | kind signatures, `Data.Kind` |
| `good/Reader`, `Sort` | `Applicative`, `forall` |
| `good/Record` | field puns |
| `good/Mismatch` | clauses with different numbers of arguments, which the Report forbids (4.4.3.1) |
| `good/WeirdClass` | a superclass context that is not a class applied to a type variable (4.3.1) |
| `good/ZeroField`, `bad/check/Mismatch` | import `Hazy.Prelude` |
| `bad/check/AmbiguousInt` | accepted by the Report: the monomorphism restriction and defaulting make it an `Integer` |
| `bad/check/DeriveAny`, `DeriveNum` | standalone deriving |
| `bad/check/DoubleApp`, `TypeClassTypeVariablePair` | contexts the Report allows |
| `bad/check/Orphan` | `instance Num [a]`: refused while `Num` had `Show` above it, which a list of anything is not. `Num` has nothing above it now, as in GHC, which accepts it |

The tests are run by [test/HazySpec.hs](../test/HazySpec.hs).
