# Powder

Toy compilers for a small Haskell. A shared front end turns source into a
little lambda calculus, Core, and each backend takes it from there.

> **A note on AI use.** Most of the code, tests and documentation here were
> written by an AI model, Anthropic's Claude working through
> [Claude Code](https://claude.com/claude-code), under the author's direction;
> the commits it wrote say so in a `Co-Authored-By` line. It is a project for
> trying ideas out, and has not been reviewed line by line. It passes its own
> tests, but expect mistakes, and do not rely on it for anything that matters.

There is one backend so far: `powder-ski`, which compiles to SKI combinators
and runs them by graph reduction, in the style of
[MicroHs](https://github.com/augustss/MicroHs).

```
$ cabal run powder-ski -- examples/lazy.hs
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71
2880067194370816120
111

$ cabal run powder-ski -- --dump-comb examples/fact.hs | grep '^fact'
fact = S (C' C (C primIntEq #0) #1) (S * (B fact (C - #1)))
```

Build with `cabal build`, test with `cabal test` (see [Testing](#testing)). It needs GHC,
[Alex](https://haskell-alex.readthedocs.io), [lambline](../lambline) checked
out beside this directory, and diagnose from Hackage.

## The REPL

`cabal run powder-ski` with no file starts a REPL; `--repl FILE` starts one
with a file loaded.

```
> product [1 .. 25]
15511210043330985984000000
> fib 0 = 0; fib 1 = 1; fib n = fib (n - 1) + fib (n - 2)
> fib 20
6765
> :comb \f g x -> f (g x)
B
> :comb fib
fib = S (C (S' C' (C primIntEq #0) (C' C (C primIntEq #1) #1)) #0) (S' + (B fib (C - #1)) (B fib (C - #2)))
```

Enter an expression to evaluate it, or declarations to define things.
`:comb` and `:core` show what an expression or a name compiles to, and
`:help` lists the rest.

The prompt follows Meadow's REPL in how it reads an entry.

```
> sign n
    | n < 0 = 0 - 1
    | otherwise = 1
sign :: (Ord a, Num a, Num b) => a -> b

> fact :: Integer -> Integer
  fact 0 = 1
  fact n = n * fact (n - 1)
fact :: Integer -> Integer

> fact 20
_ :: Integer
= 2432902008176640000

> data Colour = Red | Green deriving (Eq, Show)
defined type Colour

> zip [Red, Green] "rg"
_ :: [(Colour, Char)]
= [(Red,'r'),(Green,'g')]

> map
map :: (a -> b) -> [a] -> [b]
```

An entry says what it is, as Meadow's prompt does: a definition its type,
and an expression its type and then its value. An IO action is run. Any
other value is printed with `show`, at the type it has once a number of no
particular type has been made an `Integer`; what cannot be shown, such as a
function, is left at its type. An entry whose types are wrong is refused,
and defines nothing.

- **Enter** submits an entry as soon as it parses, so `1 + 2` takes one
  Enter. An entry that does not parse is carried on to a new line, indented
  to where the next line belongs: a unit in after `=`, `->`, `of`, `do`, an
  operator or an open bracket, and otherwise level with the line above.
- **A blank line** ends an entry of several lines, whatever it holds. Short
  of that it is submitted once it parses with nothing left dangling. An
  entry that opens with a type signature, or whose last line is a guard,
  always waits for the blank line, so that every equation gets in.
- **Alt-Enter** or **Ctrl-J** starts a new line whatever the entry looks
  like, which is how to follow one equation with another.
- **Up** and **Down** move between the lines of an entry, and from its first
  and last line through the history. The history is kept in `.repl_history`
  in the directory the REPL is started from. **Ctrl-R** searches it.
- **Tab** completes by what the cursor is on, listing the candidates when
  there are several: types in a signature or a data declaration, and in an
  expression constructors after a capital and values otherwise. It also
  completes commands and the file for `:load`.

Only the first line of an entry carries the prompt; the rest start under it.
The layout rule sees the columns you see.

The line editor is [lambline](../lambline), a port of rustyline, which
`cabal.project` expects in a directory beside this one. Its README lists the
keys.

A definition replaces any earlier one of the same name, for the code that
already calls it too.

## The build tool

`powder`, in [buildtools/powder](buildtools/powder), builds packages. A
package is a directory with a `Powder.toml`:

```toml
[package]
name = "app"
version = "0.1.0"

[dependencies]
util = { path = "../util" }
```

Its modules are under `src/`, the module `A.B` in `src/A/B.hs`, and
`src/Main.hs` is where a program starts. A package with no `Main` is a
library. A dependency is another package, by where it is; a package can
import the modules of the packages its manifest names, and of the library
the language comes with, and no others. No two packages of a build may
have a module of one name, since an import says nothing of packages; a
dependency's own `Main` is left out.

```
$ powder init app            # a manifest, src/Main.hs and a .gitignore
$ powder run app -- one two  # build, and run Main with arguments
   Compiling base (lib)
   Compiling util v0.1.0 (/home/me/util)
   Compiling app v0.1.0 (/home/me/app)
    Finished 3 compiled, 0 fresh -> app/target/debug/bytecode/app.pbc
     Running app/target/debug/bytecode/app.pbc on powder-vm
$ powder build app -j 4      # compile up to four packages at once
$ powder clean app
```

The package is the unit. Each is compiled as one thing, from all its
modules, after the packages it depends on and from what was kept of them,
not from their sources. What comes of it is kept in
`target/debug/incremental/`, with a fingerprint of what went into it: its
sources, the compiler, and the fingerprints of the packages it depends
on. A build compiles a package again only if its fingerprint has changed,
so a change is paid for by the package it is in and those above it.
Packages that do not depend on each other are compiled at once, one to a
processor unless `-j` says otherwise. The library the language comes
with is a package like any other, below all of them, and is kept in
`~/.powder` (or `$POWDER_HOME`), so it is compiled once for every build
there is.

`powder run` runs the program on `powder-vm` if it finds it, beside
itself, on the `PATH` or where `POWDER_VM` says, and otherwise on the
reducer built in; `--runtime vm` or `--runtime reducer` says which.

It is modelled on Meadow's build tool, with two things Meadow's has not:
packages are compiled in parallel, and the compiler has no sources of a
dependency before it. Not there yet: dependencies from git and a
lockfile, workspaces, profiles and features, and a `test` command.

Its tests ([buildtools/powder/test](buildtools/powder/test)) build a
small set of packages and check what is compiled again after each kind
of change; and they run the whole conformance suite the way the tool
compiles, each test as a package after the library, to know that
compiling apart comes to what compiling together does.

## The language

Haskell 2010, less what is listed under it:

- function equations with nested patterns, guards and `where`; pattern
  guards; lazy patterns; `case`, `let`, `if`, lambdas, `do`
- `data` and `newtype` declarations, with records and with constructors
  written infix; type synonyms; type signatures; `expression :: type`
- type classes: `class` with superclasses and default methods, `instance`
  with a context, contexts in signatures, classes of type constructors
  such as `Functor` and `Monad`, and `deriving` for `Eq`, `Ord`, `Show`,
  `Enum` and `Bounded`
- kinds, inferred for types and classes and checked wherever a type is
  written
- the numeric classes, with `Integer`, `Int`, `Double`, `Float` and
  `Rational`; defaulting, `default` declarations, and the monomorphism
  restriction
- modules: export lists, imports that are qualified, renamed, listed or
  hiding, `module M` in an export list, and modules that import each other
- lists, tuples, strings, ranges, list comprehensions, operator sections
- user-defined operators with `infixl`/`infixr` declarations
- the layout rule
- `foreign import ccall`, of C functions and addresses
- the Prelude, and beside it `Data.List`, `Data.Char`, `Data.Maybe`,
  `Data.Ratio`, `Data.Ix`, `Data.Array`, `Data.Bits`, `Data.Int`,
  `Data.Word`, `Data.Complex`, `Control.Monad`, `Numeric`, `System.IO`,
  `System.IO.Error`, `System.Exit`, `System.Environment`, and `Foreign`
  and the libraries under it, in [lib](lib)
- beyond the Report's libraries: `Applicative`, `Alternative`, `MonadFail`,
  `Semigroup`, `Monoid`, `Foldable` and `Traversable`, in the Prelude and
  in `Control.Applicative`, `Data.Functor`, `Data.Function`,
  `Data.Foldable`, `Data.Traversable`, `Data.Monoid` and `Data.Semigroup`;
  and `Data.List.NonEmpty`, `Data.Functor.Identity`, `Data.Functor.Const`,
  `Data.Bifunctor`, `Data.Either`, `Data.Ord`, `Data.Tuple`, `Data.Bool`
  and `Data.IORef`

A program is kind and type checked before it is compiled (see
[Types](#types)), and one that is not well typed does not run.

What is not as the Report has it:

- **Calling C** is the Report's. A foreign import takes and gives the
  basic foreign types: the numbers, `Char`, `Bool`, `Ptr`, `FunPtr`,
  `StablePtr`, the types of `Foreign.C.Types`, and newtypes and synonyms
  of those; any number of them, purely or as an action. `"&name"` is the
  address of something of C's, `"dynamic"` calls a function by its
  address, and `"wrapper"` makes a C function of a function of the
  program's, which C can then call. `foreign export` makes a function C's
  under a name. Calls are made with libffi, of a function looked up by
  name among the program's own exports, then in the C library and in
  whatever libraries `POWDER_FOREIGN_LIBS` names, between colons. The
  `Foreign` libraries are all there. What to know:
  - Sizes are those of a 64-bit system whose `long` is 64 bits, and
    `errno`'s numbers are Linux's.
  - A `ForeignPtr`'s finalizers run when it is finalized or when the
    program ends, and not sooner, since nothing finds out when the last
    use of one has been made.
  - A C function made by `"wrapper"` is kept for as long as the program
    runs: `freeHaskellFunPtr` does nothing.
  - An export is made when the program starts, so it has an address then
    and no name a C library could be linked against. `powder-ski --stubs
    FILE` writes C that gives each export its name, as a function that
    calls through a place the machine fills in; `powder build` writes it
    to `target/debug/stubs/`. Compiled into a shared library and named
    first in `POWDER_FOREIGN_LIBS`, it is what C that calls the exports
    links with.
- **Handles** are the Report's: open, semi-closed after `hGetContents`
  and closed; many readers or one writer of a file; `hLookAhead`,
  seeking, sizes, and I/O errors that say what kind they are and what
  handle and file they are about. Their state is kept by the library in
  [lib/Powder/Base.hs](lib/Powder/Base.hs), over a few things the
  machines do. Two things are only kept and given back: how a handle is
  buffered (`hFlush` does send on what is held), and a file is known to
  be locked by its name, where GHC knows it by what file it is.

Reading and showing are the Report's own definitions: `lex`, the escapes
of characters and strings, `Read` and `Show` for the Prelude's types, and
`Ix`. Tuples go up to fifteen, as the Report says they must, with `Eq`,
`Ord`, `Bounded`, `Read`, `Show` and `Ix` at each size. `getContents`,
`readFile`, `hGetContents` and `interact` read as the string is looked at,
so a program that answers its input line by line does so as the lines
come.

Everything a program is made of is in [lib](lib), written in the language
itself. [Powder.Base](lib/Powder/Base.hs) holds the definitions, and
`Prelude`, which every module imports unless it says otherwise, is a
selection from it; the library modules are selections and additions in
the same way.

There is one library for every edition, as GHC has, and its classes are
the ones programs are written against today and not the Report's.
`Applicative` is above `Monad`, `fail` is `MonadFail`'s and not
`Monad`'s, and `length`, `foldr`, `mapM_`, `sequence` and their kind are
`Foldable`'s and `Traversable`'s, so they go over more than lists. What
an edition chooses is the Prelude's names. In `Haskell2010` the Prelude is
[Powder.Prelude2010](lib/Powder/Prelude2010.hs): the names the Report
gives it and no others, so that a program of the Report's may still call
something of its own `pure`, `<>` or `traverse`. In every other edition
it is [Prelude](lib/Prelude.hs), which has these classes too, as GHC's
does. The extension
`ReportPrelude`, which is Powder's own and which `Haskell2010` turns on,
is what says which. So four things in a Haskell 2010 program are not as
the Report has them: an instance of `Monad` needs one of `Applicative`,
`fail` cannot be defined in an instance of `Monad`, the types of the
Prelude's folds are more general, and `Num` has neither `Eq` nor `Show`
above it, so a function that compares or shows the numbers it is given
says so in its type.

`Prelude` and
`Powder.Base` import each other, which is the first use the compiler
makes of modules being allowed to.

The examples stick to the subset that GHC also accepts, so the expected
outputs in `examples/*.out` come from `runghc`.

## Editions and extensions

Beyond the Report the language is in extensions, as GHC's is, and an
edition is a set of them under a name. A module is written in the
edition asked for where it is compiled, with any extensions asked for
beside it turned on or off; and then in what its own pragmas say, each
naming an extension, one to turn off with `No` before it, or an edition
to start again from:

```haskell
{-# LANGUAGE LambdaCase, NoTupleSections #-}
```

| Edition | What it is |
| --- | --- |
| `Powder2026` | The default: the extensions it is thought a program should be written with. Still being chosen. |
| `Haskell2010` | The Report's language, and a Prelude of the Report's names only (`ReportPrelude`). The libraries and the conformance suite are in it. |
| `GHC2021`, `GHC2024` | GHC's, as far as their extensions are implemented. |

Where it is said: `powder-ski --edition NAME -XEXTENSION FILE`; in a
package, `edition = "..."` and `extensions = [...]` under `[package]` in
`Powder.toml`, which is for every module of the package; and in a file,
by pragmas, which are for that file and come after the package's.

Every extension of GHC2024 is known by name
([Powder.Extensions](src/Powder/Extensions.hs)). An edition turns on
those of its extensions that are implemented; asking for one that is
not, by name, is an error, so that a program that needs it is not
compiled as if it did not. Implemented so far, all of them in
`Powder2026`:

| Extension | What it adds |
| --- | --- |
| `LambdaCase` | `\case` alternatives: a function that takes its argument apart |
| `TupleSections` | `(,x)`: a tuple with places left out is a function of them |
| `EmptyCase` | `case x of {}` |
| `MultiWayIf` | `if \| guard -> e \| ...` |
| `BlockArguments` | `f do ...`, `g \x -> ...`: a block as an argument, as it stands |
| `ImportQualifiedPost` | `import M qualified` |
| `NumericUnderscores` | `1_000_000` |
| `BinaryLiterals` | `0b1010` |
| `HexFloatLiterals` | `0x1.8p3`: a fraction in hexadecimal, with a power of two |
| `NamedFieldPuns` | `C {f}`: a field by itself is the variable of its name |
| `BangPatterns` | `!pattern`: what is matched is worked out first. On a binding in a `let` or a `where`, it is worked out before what the binding is for |
| `PostfixOperators` | `(e op)` is `op e`, so `op` may take one argument. It is so with the extension off too |
| `EmptyDataDeriving` | `data Void deriving (Eq, Show)` |
| `GADTSyntax` | `data T a where C :: a -> T a`: constructors given by their types. Each must make the type applied to variables, which is all that is not a GADT |
| `RecordWildCards` | `C {..}`: the fields not named, each as the variable of its name. Not in a pattern bound at the top of a module |
| `DisambiguateRecordFields` | A field that two modules have is the constructor's, where a record is built or matched |
| `ExplicitNamespaces` | `import M (type T)`: `type` before a name in a list of names |
| `ViewPatterns` | `(f -> p)`: matches what `p` does, of `f` applied to what is matched. Where patterns are matched, not where one is bound |
| `PatternSynonyms` | `pattern P x = C x 0`: a name for a pattern, and an expression too; `pattern P x <- ...` for a pattern only, with equations after `where` to say how it is built. Exported and imported as `pattern P` |
| `ParallelListComp` | `[e \| x <- xs \| y <- ys]`: rows of qualifiers gone through side by side |
| `PackageImports` | `import "base" Data.List`. The package is not looked at |
| `UnicodeSyntax` | `→`, `←`, `⇒` and `∷` for `->`, `<-`, `=>` and `::` |
| `DeriveFunctor`, `DeriveFoldable`, `DeriveTraversable` | `deriving (Functor, Foldable, Traversable)`, over a type's last parameter: through lists, tuples and other functors, and for `Functor` what a function gives |
| `StandaloneDeriving` | `deriving instance Show T`, apart from the declaration of `T`, which is in the same module |
| `DerivingStrategies` | `deriving stock (Eq)`, `deriving newtype (Num)`, `deriving anyclass (C)`: how, said |
| `GeneralisedNewtypeDeriving` | A newtype has the instances of what it is a newtype of: `newtype Age = Age Int deriving (Num)`. For a class of type constructors too, where the newtype is of something applied to its last parameter |
| `ExplicitForAll` | `f :: forall a b. a -> b -> a`: a signature names its type variables |
| `ScopedTypeVariables` | The variables a `forall` names are in scope in the definition, an instance's in its methods and a class's in its defaults; and a pattern may be said to have a type, `(x :: Int)` |
| `TypeApplications` | `read @Int "5"`: what the first of a name's type variables are. `@_` leaves one to be found |
| `InstanceSigs` | An instance may say a method's type again |
| `ConstrainedClassMethods` | A method's context may ask more of the class's variable |
| `KindSignatures`, `StandaloneKindSignatures` | `data T (f :: Type -> Type) a`, `(t :: Type)`, and `type T :: Type -> Type` apart from the declaration. A kind is `*` or `Type`, with arrows |
| `TypeOperators` | `data a :+: b`, `type a ~> b = a -> b`, and `Int :+: Bool` in a type: tighter than the arrow, and grouped by their fixities |
| `MultiParamTypeClasses` | `class Convert a b`: a class of several types, or of none |
| `FunctionalDependencies` | `class Container f a \| f -> a`: some of a class's types say what others are |
| `FlexibleInstances`, `TypeSynonymInstances` | `instance C [Char]`, `instance C (Maybe Int)`, `instance C String`: an instance at any type. Where two fit, `{-# OVERLAPPING #-}`, `{-# OVERLAPPABLE #-}` and `{-# OVERLAPS #-}` say the more particular is taken |
| `FlexibleContexts` | `Show [a] => ...`: a class asked of any type |
| `ConstraintKinds` | `type Stringy a = (Show a, Read a)`: a name for constraints |
| `ExistentialQuantification` | `data Showable = forall a. Show a => MkShowable a`: a constructor with type variables of its own, and a context, whose dictionaries what it makes carries |
| `RankNTypes` | `f :: (forall a. a -> a) -> (Int, Bool)`: an argument, or a field, that works at every type |

Implemented and in no edition, since each changes what a program already
means:

| Extension | What it does |
| --- | --- |
| `StrictData` | A constructor's fields are strict unless marked lazy, `~T` |
| `LexicalNegation` | A minus that touches what follows it and not what comes before is that thing's sign: `f -1` is `f (-1)`, and `(- 1)` is a section |
| `NumDecimals` | `1e6 :: Int`: a number written with an exponent, if it is whole, is a whole number |
| `DeriveAnyClass` | `deriving (C)` for a class that cannot be derived is an instance with no methods |
| `OverloadedStrings` | A string written in a program is of any type in the class `IsString`, of `Data.String` |
| `PartialTypeSignatures`, `NamedWildCards` | `f :: _ -> Int`: a type with parts left out, which are found |
| `AllowAmbiguousTypes` | A method's type need not mention its class's variable: applying it to a type says which instance is meant |
| `UndecidableInstances` | Accepted, and says nothing: no instance is checked to be one whose use must end |

And one of Powder's own, which only `Haskell2010` has on:

| Extension | What it does |
| --- | --- |
| `ReportPrelude` | The Prelude has the names the Report gives it and no others |

An extension is tested like the Report is, in
[conformance](conformance): a directory `ext-NAME` of programs that turn
it on by a pragma and must print what GHC prints, and one under `reject`
of programs that must be refused without it.

## How it fits together

| Stage | Module | |
| --- | --- | --- |
| Lexing | [Powder.Lexer](src/Powder/Lexer.x) | An Alex lexer for the lexical syntax of Haskell 2010. It keeps everything: whitespace and comments are tokens, and what is no token is an error token, so the tokens always spell the input. An operator is told by what touches it, as GHC's lexer tells it: see below. |
| Layout | [Powder.Layout](src/Powder/Layout.hs) | A pass of its own, adding the braces and semicolons indentation implies as tokens with no text. |
| Parsing | [Powder.Parser](src/Powder/Parser.hs) | Hand-written recursive descent with lookahead and no backtracking, building a lossless tree. It always gives a tree, with error nodes where the input is wrong. |
| Syntax trees | [Powder.Syntax.Green](src/Powder/Syntax/Green.hs), [Red](src/Powder/Syntax/Red.hs) | Homogeneous trees: a green tree is kinds, widths and text; a red tree adds offsets and parents. |
| Typed syntax | [Powder.Syntax.AST](src/Powder/Syntax/AST.hs) | A typed view of the tree: pattern synonyms over view patterns, with `COMPLETE` pragmas. |
| Type checking | [Powder.Typecheck](src/Powder/Typecheck.hs) | Bidirectional checking over HIR, with type classes, giving tables by node: types, and the dictionaries to pass. |
| Name resolution | [Powder.Resolve](src/Powder/Resolve.hs) | Modules' scopes, a little desugaring, and every name replaced by the identity of what it refers to. Gives HIR. |
| HIR | [Powder.HIR](src/Powder/HIR.hs) | The program in the shape of its syntax, with an identity on every node and every name, and side tables keyed by them. |
| Lowering | [Powder.Lower](src/Powder/Lower.hs) | HIR to Core: pattern-match compilation, guards, and dependency analysis of `let`. |
| Core | [Powder.Core](src/Powder/Core.hs) | Lambda calculus with `let`, constructors, flat `case`, and primitives. Untyped, for now. |
| Errors | [Powder.Diagnostics](src/Powder/Diagnostics.hs) | Problems with their place in the source, shown with [diagnose](https://hackage.haskell.org/package/diagnose). |

[Powder.Frontend](src/Powder/Frontend.hs) runs those and hands a backend a
`Program`: the definitions reachable from `main`. A new backend starts there.

### Layout and the parser

The layout rule has one clause the layout pass cannot apply: an implicit
block ends where the next token would otherwise be a parse error, which is
what closes the `let` block in `let x = 1 in x`. So the pass answers both
ways. Its output is a stream in which every token inside an implicit block
comes with a second stream, the tokens as they would be had the block been
closed just there. The parser reads the first, and its `expect` switches to
the second when it wanted a closing brace and found something else. That is
the only place the parser steers the layout. The idea is from
[Hazy](https://github.com/Superstar64/Hazy).

The parser follows Alex Kladov's
[resilient LL parsing](https://matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html):
each function looks ahead to decide what it is at, and records `open`,
`close` and `advance` events from which the tree is built. Infix expressions
are kept flat, as written, and grouped by precedence in the desugarer, since
the parser does not know fixities.

### Operators and what touches them

The lexer sorts each occurrence of an operator the way GHC's does, by
whether something it could apply to is hard against it on each side: a
name, a literal or a closing bracket before it, a name, a literal or an
opening bracket after. With something after and not before it is a
*prefix* occurrence, the other way about a *suffix* one, with both a
*tight infix* one, and with neither a *loose infix* one. Alex decides it,
with a predicate on what is to the left of the token and one on what is to
the right.

Two operators mean something as a prefix occurrence that they do not
otherwise: `~` before a pattern makes it lazy, and `!` before a type makes
a field strict. So `f ~(a, b)` has a lazy pattern, and `a ~ b`, `a~b` and
`(~)` are the operator. Everything else is the same operator however it
sits, so that Haskell 2010 source means what it always did; in particular
a minus sign is not read differently for being hard against a number.

### Kinds

[Powder.Kinds](src/Powder/Kinds.hs) finds the kind of every type
constructor and class, and checks every type written against them. It
takes the declarations of types and classes in groups that depend on each
other, works out the kinds in a group together by unification, and makes
what is still undetermined when a group is done `*`, as the Report says.
It runs before the type checker, on the same HIR, and its errors come
first, since a type put together wrongly is usually why its uses go wrong.

### Reading the tree

The tree can hold anything anywhere, so code reads it through the typed
layer. Each sort of syntax is a type wrapping a red node, and each form is a
pattern, in a module to import qualified:

```haskell
import qualified Powder.Syntax.AST.Expr as Expr

size :: Expr -> Int
size = \case
  Expr.Var _ -> 1
  Expr.App f args -> size f + sum (map size args)
  Expr.Paren e -> size e
  ...
```

Leave a form out and GHC says so, as it would for a data type. `syntax`
gives back the node underneath, with its text, position and neighbours.

### HIR

Name resolution turns syntax into HIR, which keeps the shape of the source
less a little sugar: operators are applications grouped by their fixities,
sections and `do` are lambdas and applications, tuples and lists are
constructors, and the equations of a function are one binding.

What it adds is identity:

- Every **name** has a `NameId`: each top-level definition, constructor and
  local variable. A use of a name is the identity of its binder, so scope
  is settled. Two variables spelled alike in different scopes are different
  names.
- Every **node** has a `NodeId`: each expression, pattern, binding, clause,
  right-hand side and alternative.

So nothing else has to live in the tree. What is known about a node or a
name is in a table beside it, keyed by its identity: `programOrigins` says
where each node came from in the source, and `programNames` what each name
is. `--dump-hir` shows a file's HIR, each name with its identity:

```
$ cabal run powder-ski -- --dump-hir examples/fact.hs
fact#125 0 = 1
fact#125 n#2258 = *#17 n#2258 (fact#125 (-#16 n#2258 1))
```

Resolution reports everything it finds rather than stopping at the first
mistake: names not in scope, unknown constructors, a name defined twice, a
variable bound twice in one set of patterns.

### Types

[Powder.Typecheck](src/Powder/Typecheck.hs) checks HIR and gives back a
table: the type of each node, under its `NodeId`. Nothing is written into
the tree.

It is bidirectional, as GHC's checker is. An expression is either asked for
its type or told it. Telling is how a signature's type reaches the lambdas,
branches and alternatives inside a definition, so that a mistake is reported
against what was declared. Where nothing is told, types are found by
unification, and generalised at each definition with no signature.

```
$ cabal run powder-ski -- --typecheck examples/lazy.hs
primes :: [Integer]
fibs :: [Integer]
collatz :: Integer -> [Integer]
main :: IO ()
```

In the REPL, `:type EXPR` shows an expression's type.

Unification is by union-find with path compression, in `ST`
([Powder.Unify](src/Powder/Unify.hs)): an unknown is a node in a forest,
two unknowns found to be the same are joined, and the root of a tree holds
what its unknowns turned out to be. The checker's own notes are references
too; there is no state monad in it. When it is done the types are read out
into the plain ones of [Powder.Types](src/Powder/Types.hs).

#### Classes

Classes are checked the way they will be compiled, by passing
dictionaries, but the checker passes nothing: it leaves notes, in two more
tables beside the tree.

- Where an overloaded name is used, its type's context is *wanted* of the
  types it is used at. When a type is known, what is wanted of it is seen
  to: a type built by a constructor needs an instance for the constructor,
  which may want things of its arguments in turn, and a type variable has
  only what a signature gives it, and what that brings by way of
  superclasses. The reason found is *evidence*, kept under the node of the
  use: a parameter, an instance applied to evidence, or a superclass out of
  other evidence.
- What is wanted of a type that is still unknown when a definition is
  generalised becomes the definition's context, and the definition is
  noted to take a dictionary for each class in it. A definition with a
  signature takes those its signature says, and may use only those.
- Under the monomorphism restriction, which holds for a definition that is
  not written as a function and has no signature, such a type is not
  generalised but left to be settled by how the definition is used.
- What is still wanted when the whole program has been checked is
  defaulted: a type that must be a number is an `Integer`. Anything else
  left over is ambiguous, and reported.

A number written in a program is `fromInteger` of it, at whatever numeric
type it is used at, except where that turns out to be `Integer` or `Int`,
which is noted so that nothing is done at run time.

[Powder.Lower](src/Powder/Lower.hs) turns the notes into code. A class is
a data type with one constructor, whose fields are the dictionaries of its
superclasses and then its methods; a method is the function that takes its
field out; an instance is a definition that builds a dictionary, given
those its context asks for; evidence becomes an argument, and a noted
parameter a lambda.

```
$ cabal run powder-ski -- --typecheck examples/classes.hs
insert :: Ord a => a -> Tree a -> Tree a
toList :: Tree a -> [a]
total :: Num a => [a] -> a
safeDiv :: Int -> Int -> Maybe Int
main :: IO ()
```

### Modules and packages

The compiler takes a package: every module there is, at once.
`powder-ski FILE` makes one of the prelude, the file, and every module
that imports lead to from it, each looked for from the file's directory by
its name, `A.B` in `A/B.hs`. There is no compiling a module by itself, and
so nothing like an interface file to write for one.

That is what lets modules import each other, which the Report allows and
which here needs nothing like GHC's `.hs-boot` files.
[Powder.Resolve](src/Powder/Resolve.hs) first gives every top-level name
of every module its identity. What is in scope in a module is then its own
names and those of what its imports take from the exports of other
modules, and what a module exports is picked out of what is in scope in
it, so the two are found together: every module's exports start empty and
are worked out again from the others' until none changes.
[examples/modules.hs](examples/modules.hs) has two modules that import
each other, one of which exports the other on.

After that a name is an identity and modules are gone: the type checker
orders definitions by what they use across the whole package. In Core a
name of a module other than the last is spelled with its module before it,
`Prelude.map`.

What is typed at the REPL is part of a module: the file's that was loaded,
or `Main`. So the prompt sees all of that module, exported or not, and a
definition entered again replaces the earlier one for the code that
already calls it.

A package can be compiled after others, without their sources:
[Powder.Package](src/Powder/Package.hs). What comes of compiling one is
its code, in Core, and an interface: what its modules export, its types,
classes and instances without their code, and the type of everything it
defines. A package is given the interfaces of all the packages below it,
which are put back into the tables name resolution and the type checker
start from, and its own interface says only what it adds. Identities
made by packages compiled apart must not be the same, so each package
makes its names and nodes in a run of numbers of its own. In a package
every name is spelled with its module's, and a program is the code of its
packages put together, started at `Main.main`.

That is what [the build tool](#the-build-tool) is made on. In time the
compiler will keep what it works out within a package too, by the groups
of modules that import each other.

Scope is the Report's. A name that a module defines and also imports is
ambiguous where it is used unqualified, unless the import hides it. A
fixity belongs to the operator it is declared for, and goes where the
operator goes. An instance is in scope only where a chain of imports leads
to its module. And types are settled a group of modules at a time, the
groups being the modules that import each other, taken in the order of
what imports what: the checker takes a group's definitions, classes and
instances together, defaults what is left undetermined, and only then
looks at the next, so what a later module does with a name cannot change
the name's type. Those groups are the units the compiler will cache by.

The one liberty is at the REPL, where a definition hides an imported name
of the same spelling, since defining again is what a prompt is for.

### Where this is going

The target is Haskell 2010, and after that extensions, up to GHC2024.
Next:

1. **More of [the build tool](#the-build-tool)**: dependencies from git
   with a lockfile, workspaces, tests, and keeping what is compiled within
   a package, by the groups of modules that import each other.
2. **Lowering to a typed Core** like GHC's, from HIR and the tables, in
   place of the untyped Core here now.

### Conventions

`StrictData` is on throughout; a field that has to be lazy says so with `~`,
as the layout stream's two continuations do. The source is kept free of
hlint hints.

### The sequent calculus backend

The combinator reducer below is the reference: what a program does on it
is what the program means. Beside it is a second backend, which is to be
the real one. It goes from Core through two IRs to a bytecode, all in the
compiler, and a machine written in Rust runs the bytecode.

| Stage | Where | |
| --- | --- | --- |
| Sequent calculus | [Powder.Sequent](src/Powder/Sequent.hs), [FromCore](src/Powder/Sequent/FromCore.hs) | The λμμ̃ calculus, call by value: producers, consumers, and statements that cut one against the other. |
| AxCut | [Powder.AxCut](src/Powder/AxCut.hs), [Lower](src/Powder/AxCut/Lower.hs) | Every cut reduced until a variable stands on one side: statements only, over one environment that is a row of values with no names. |
| A machine for AxCut | [Powder.AxCut.Machine](src/Powder/AxCut/Machine.hs) | What AxCut means, for testing the lowering by itself. Not the runtime. |
| Bytecode | [Powder.Bytecode](src/Powder/Bytecode.hs) | AxCut written down: values by their place in the environment, code cut into blocks. |
| The machine | [runtime/](runtime/src/main.rs) | Rust. Loads an image and runs it. |

```
$ cabal run powder-ski -- --axcut examples/sort.hs          # on the machine for AxCut
$ cabal run powder-ski -- --emit sort.pbc examples/sort.hs  # write the bytecode
$ cargo run --release --manifest-path runtime/Cargo.toml -- sort.pbc
$ cabal run powder-ski -- --dump-sequent examples/hello.hs  # or --dump-axcut
```

**Laziness is written out.** The calculus is strict, so what Haskell
leaves unevaluated is a *thunk*: an object of one method, to force it,
which the machine replaces with its value. A variable stands for a thunk
or a value, and to look at one it is forced. So every thunk made and every
force is in the program to be seen, which is what an optimisation about
laziness needs: to find that a thunk is always forced, or is cheap to work
out at once, is to delete it. The translation from Core does the first of
these already, in the one case that needs no analysis: a variable, a
number, a lambda, an action and a constructor applied to all its fields
are values as they stand, and are never thunks.

**Actions are compiled away.** In Core an action is a primitive left for
the runtime to interpret. Here it is something to run, given a consumer
for its result and one for what it raises; sequencing, raising and
catching are how those two consumers are passed along, and the only
primitives left are the ones that touch the world.

**The environment is linear.** In AxCut a statement works on the end of
the row of values: `let` makes the last few one value, `switch` takes the
last apart, `new` captures the last few in an object, `invoke` calls a
method of the last with the rest as arguments. A value is used once, and
to use it twice or not at all is a `substitute` that makes the row again.
The lowering works out, from the inside of each definition outwards,
what is still wanted after every statement, and arranges the row to be
exactly that. So the machine counts references by the statements alone:
a count goes up where a `substitute` copies, and a value is let go where
one leaves it out.

The machine in Rust counts references, as above, and runs in constant
stack. A count frees a value the moment the last reference to it is let
go of, but values that refer to each other in a circle, which lazy code
makes (`let xs = 1 : xs`, or two local functions that call each other),
keep each other's counts up for good. Those are freed by a cycle
collector ([runtime/src/gc.rs](runtime/src/gc.rs)), Bacon and Rajan's
trial deletion: a value whose count goes down but not to nothing is
noted as a possible way into a circle; to collect, the counts of
everything those reach are taken down by the references among them, and
what is left with no count is garbage. It looks only at what the
possible roots reach, never at the whole heap, and runs between
instructions when enough roots have gathered, less often when a look
finds little. `POWDER_GC_STATS=1` has the machine say, as it leaves, how
many values it made and how each was freed, and `POWDER_GC=off` leaves
circles be.

A tracing collector as an alternative is to come, and after it a JIT and
ahead-of-time compilation to native code, from the same bytecode. The
machine is as fast as the reducer or faster on most programs, and up to
two and a half times slower on the ones that allocate most. What time is
left is spread over the whole loop that runs instructions, so the next
gains are in what the compiler gives it to run: fewer rows made again,
and fewer thunks where a value is known to be wanted.

Every example is run on all three, the reducer, the machine for AxCut and
the machine in Rust, and they must agree.

### The SKI backend

[Powder.SKI.Compile](src/Powder/SKI/Compile.hs) removes every variable from
Core:

- **Lambdas** go by bracket abstraction. `\x -> e` becomes a term built from
  combinators that, applied to an argument, puts it wherever `x` was.
- **Data** is Scott-encoded, so constructors are functions and `case` is
  application: `x : xs` is `\nil cons -> cons x xs`.
- **Local recursion** uses `Y`. Top-level definitions refer to each other by
  name instead, and become a cyclic graph.

[Powder.SKI.Eval](src/Powder/SKI/Eval.hs) is the runtime: a graph of mutable
nodes, reduced to weak head normal form by unwinding the spine and
overwriting each redex with its result. Overwriting is what gives sharing,
and so laziness. IO actions are inert primitives that a small interpreter
loop executes.

The target is the combinator set MicroHs uses, and the abstraction rules are
a port of its `MicroHs.Abstract`. Beyond S, K and I there are B, C and their
primed forms S', B', C', which keep abstraction over several variables from
blowing up, and a handful for common shapes: A, U, Z, P, R, O, K2, K3, K4
and C'B. Several are exactly the Scott encodings of everyday data: `True` is
A, `(,)` is P, `(:)` is O. [Powder.SKI.Comb](src/Powder/SKI/Comb.hs) lists
the reduction rules.

`--stats` prints program size and reduction count, `--dump-hir` the program
after name resolution, `--dump-core` the Core, and `--dump-comb` the
combinators.

## Testing

`cabal test` runs one [hspec](https://hspec.github.io) suite, in
[test/](test):

- **Unit specs** for the prompt's entry rule, the lexer and parser, name
  resolution and the type checker.
- **The examples**, each run on all three runtimes and compared, with
  [hspec-golden](https://hackage.haskell.org/package/hspec-golden),
  against the `.out` file beside it, which is what GHC makes it print.
  The machine in Rust is tested if `runtime/` has been built. The types
  found for each example's definitions are golden files too, in
  [test/golden/types](test/golden/types).
- **Diagnostics**: each program in [test/errors](test/errors) is wrong,
  and what the compiler says of it is a golden file in
  [test/golden/errors](test/golden/errors).
- **Conformance**: the suite in [conformance/](conformance), below.

A golden file that is not there is written; one that differs fails, and
the new text is left beside it to compare.

### The conformance suite

[conformance/](conformance) is about 760 small programs written from the
Haskell 2010 Report, a directory to a chapter (expressions, declarations,
modules, the predefined types and classes, input and output, the foreign
function interface, the Prelude, syntax and layout, deriving) and one to
each library of Part II. Under `conformance/reject` are programs a
compiler must refuse. [conformance/README.md](conformance/README.md) says
which sections each file is for.

What a program should print is what GHC prints for it under
`-XHaskell2010`. That is kept beside each test as a `.out` file, so the
suite needs no GHC to run; `conformance/run.sh --write-expected` makes
those files again, and does need it. Where GHC and the Report differ the
test is named `-ghc-differs`, and a few carry their own
`expected-output.txt`: modules that import each other, which the Report
allows and GHC does not compile without help.

[conformance/KNOWN-FAILURES.txt](conformance/KNOWN-FAILURES.txt) lists the
tests that do not pass yet. `cabal test` passes those over, and fails if
one of them starts passing, so the list stays true. As this is written
the list is empty: all 800 or so pass, on the combinator reducer and on
the Rust machine.

Some of the tests are on ground that
[Hazy](https://github.com/Superstar64/Hazy)'s tests cover, where that is
the Report's and not an extension's: class defaults, derived instances,
fields shared between constructors, clauses over strings and numbers,
tabs in layout, a module that imports itself. Those are written anew.

Hazy's own tests that are Haskell 2010 are in [hazy](hazy) as Hazy has
them, and `cabal test` runs them too: programs that must print what Hazy
says they print, modules that must be accepted, and modules that must be
refused. [hazy/README.md](hazy/README.md) says which were left out and
why. They found, among other things, that tabs were not counted as the
layout rule counts them and that a record's field could not be an
operator.

## Licence

Powder is under the BSD 3-Clause licence: see [LICENSE](LICENSE). The
tests in [hazy](hazy) are not Powder's: they are Hazy's, under the GNU
General Public License version 3, and are kept apart, with that licence
beside them.

## Ideas for what next

- Other backends from the same Core: a CEK or STG machine, closure conversion
  to C, supercombinators with lambda lifting.
- For the SKI backend: Kiselyov's linear-time abstraction, a runtime in C
  with its own garbage collector, a serialised combinator format.
