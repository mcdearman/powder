# Haskell 2010 conformance suite

Tests derived section by section from the Haskell 2010 Language Report. Every test is a small
strict-Haskell-2010 program (no extensions, only Report modules) whose stdout is compared with
the output of the same program run with `runghc -XHaskell2010` (not with `-w`: on GHC 9.10 and 9.12 that
flag made instances of `Eq` and `Ord` for `Maybe` go missing).

## Layout

- `03-expressions/` ... `11-deriving/`: Report Part I, one directory per chapter. `10-syntax/` covers chapters 2 and 10
  (and chapter 12, pragmas). File names start with the Report section number.
- `lib-<Module>/`: Report Part II libraries, one directory per module. The modules that are not the Report's
  (`Control.Applicative`, `Data.Functor`, `Data.Function`, `Data.Foldable`, `Data.Traversable`, `Data.Monoid`,
  `Data.List.NonEmpty` and the small modules tested with it) have
  directories of the same kind; a program there that needs the Prelude of a later edition says `GHC2021` in a
  `LANGUAGE` pragma. `lib-Prelude/` is about which names the Prelude has in which edition.
- `05-modules/<test>/`: every module test is a directory; compile `Main.hs` with the directory as the import root.
- `ext-<Extension>/`: language extensions, one directory per extension. Each program turns its extension on
  with a `LANGUAGE` pragma, and is otherwise run as Haskell 2010. `ext-pragmas/` is about the pragmas themselves.
- `reject/<same dirs>/`: programs a conforming compiler must REJECT (compilation fails). No expected output.
  In `reject/05-modules/` each test is again a directory with `Main.hs`.

## Conventions for the runner

- `<name>.in` next to `<name>.hs` is fed to stdin.
- Programs in `lib-System.Environment/` are run with exactly two arguments: `one` and `two words`.
- `...-exit<N>.hs` must terminate with exit status N (stdout is still compared); all others exit 0.
- `.lhs` files (`10-syntax/10.4-*`) are literate sources (Report 10.4).
- Tests write temporary files only under `/tmp/powder-conformance-<testname>`.
- Only stdout is compared. Each result is printed on its own labelled line.
- `10-syntax/2.2-unicode-in-literals-and-names.hs` is the only source with non-ASCII characters (UTF-8); its output is ASCII.
- `10-syntax/2.7-layout-tabs-are-8-columns.hs` and `10-syntax/2.6-string-gaps-ghc-differs.hs` contain TAB characters on purpose.

## Files where GHC differs from the Report (`-ghc-differs`)

Expected output for these cannot simply be taken from GHC. Where a test of this name has a `.out` beside it,
that is what the Report says it prints, and `run.sh` leaves it alone.

- `05-modules/5.7-mutually-recursive-modules-functions-ghc-differs`
- `05-modules/5.7-mutually-recursive-modules-three-cycle-ghc-differs`
- `05-modules/5.7-mutually-recursive-modules-types-ghc-differs`
- `06-predefined/6.3.3-show-negative-double-nested-ghc-differs.hs`
- `06-predefined/6.4.2-gcd-zero-zero-ghc-differs.hs`
- `09-prelude/9-func-numeric-gcd-zero-zero-ghc-differs.hs`
- `lib-Data.Bits/bitSize-ghc-differs.hs`
- `lib-Numeric/readFloat-incomplete-lexemes-ghc-differs.hs`: the Report's `readFloat` reads no `5.` or `1e`, skips
  no space, and reads `NaN` and `Infinity`.
- `lib-Numeric/show-float-rounding-ties-and-zero-digits-ghc-differs.hs`: the Report's `showEFloat (Just 0)` shows one
  digit after the point.
- `lib-System.IO.Error/non-report-ioeGetErrorType-type-predicates-modifyIOError-ghc-differs.hs`: names GHC exports
  that the Report's list does not have.
- `10-syntax/2.4-identifiers-ghc-differs.hs`: `forall` is an ordinary name in Haskell 2010; GHC 9.10 and later
  refuse it.
- `10-syntax/2.6-string-gaps-ghc-differs.hs`: a tab is white space in a string gap; GHC 9.12 refuses it. Its
  `.out` is GHC 9.10's.
- `lib-Prelude/haskell2010-names-are-free-ghc-differs.hs`: a Haskell 2010 program that calls things of its own
  `pure`, `<>`, `traverse` and so on. GHC has one Prelude under every language, and calls the names ambiguous.

The three `05-modules/5.7-mutually-recursive-modules-*-ghc-differs/` tests are import cycles, which the Report allows
but GHC only compiles with `.hs-boot` files; each has a hand-derived `expected-output.txt`.

## Deliberately not tested

- User-defined `Monad` instances as the Report has them, with no `Applicative` instance and with `fail` among the
  methods: neither GHC nor Powder takes them. `lib-Control.Applicative/user-monad-instance.hs` has them as they are
  written now.
- `Prelude.catch` and `System.IO.Error.catch`/`try`: absent from GHC; `catchIOError`/`tryIOError` are used instead.
- FFI beyond `foreign import ccall` of numeric C functions (no pointers, `foreign export`, `&`, `dynamic`, `wrapper`),
  and the `Foreign.*` libraries of Part II.
- Runtime errors (`error`, pattern-match failure, `head []`, ...): messages are implementation-defined.

## Coverage table: Report section -> tests


### `03-expressions/` (48 accept, 0 reject)

| Report section | Accept tests | Reject tests (`reject/03-expressions/`) |
|---|---|---|
| 3.1 | `3.1-errors-bottom-never-forced.hs`<br>`3.1-errors-lazy-structures.hs` |  |
| 3.2 | `3.2-backquoted-functions.hs`<br>`3.2-constructors-as-values.hs`<br>`3.2-literals-overloading.hs`<br>`3.2-operators-as-values.hs` |  |
| 3.3 | `3.3-curried-application.hs`<br>`3.3-lambda-patterns.hs` |  |
| 3.4 | `3.4-operator-application-precedence.hs`<br>`3.4-unary-minus-is-prelude-negate.hs`<br>`3.4-unary-minus.hs` |  |
| 3.5 | `3.5-sections-and-fixity.hs`<br>`3.5-sections-backquoted-and-minus.hs`<br>`3.5-sections-left-right.hs` |  |
| 3.6 | `3.6-conditionals-optional-semicolons.hs`<br>`3.6-conditionals.hs` |  |
| 3.7 | `3.7-lists.hs` |  |
| 3.8 | `3.8-tuples-constructors.hs`<br>`3.8-tuples-large-15.hs` |  |
| 3.9 | `3.9-unit-and-parenthesized.hs` |  |
| 3.10 | `3.10-arithmetic-sequences-char-bool-ordering.hs`<br>`3.10-arithmetic-sequences-fractional.hs`<br>`3.10-arithmetic-sequences-infinite.hs`<br>`3.10-arithmetic-sequences-int-integer.hs`<br>`3.10-arithmetic-sequences-user-enum.hs` |  |
| 3.11 | `3.11-list-comprehensions-guards.hs`<br>`3.11-list-comprehensions-local-lets.hs`<br>`3.11-list-comprehensions-nested-generators.hs`<br>`3.11-list-comprehensions-refutable-patterns.hs`<br>`3.11-list-comprehensions-shadowing.hs`<br>`3.11-list-comprehensions-translation.hs` |  |
| 3.12 | `3.12-let-functions-guards-scope.hs`<br>`3.12-let-lazy-pattern-bindings.hs`<br>`3.12-let-local-fixity.hs`<br>`3.12-let-polymorphism-vs-lambda.hs`<br>`3.12-let-recursive-and-mutual.hs` |  |
| 3.13 | `3.13-case-guards-fall-through.hs`<br>`3.13-case-nested-patterns.hs`<br>`3.13-case-scrutinee-types-and-layout.hs`<br>`3.13-case-shadowing-and-scope.hs`<br>`3.13-case-where-on-alternatives.hs` |  |
| 3.14 | `3.14-do-io-sequencing-and-syntax.hs`<br>`3.14-do-list-monad.hs`<br>`3.14-do-maybe-monad.hs`<br>`3.14-do-nested-let-shadowing.hs`<br>`3.14-do-pattern-match-failure-calls-fail.hs`<br>`3.14-do-single-expression-any-type.hs`<br>`3.14-do-translation-identities.hs` |  |

### `04-declarations/` (55 accept, 0 reject)

| Report section | Accept tests | Reject tests (`reject/04-declarations/`) |
|---|---|---|
| 4.1.1 | `4.1.1-kinds-higher-kinded-parameters.hs`<br>`4.1.1-kinds-higher-order-kind-data.hs` |  |
| 4.1.2 | `4.1.2-types-partial-application-in-instances.hs`<br>`4.1.2-types-prefix-type-constructors.hs` |  |
| 4.1.3 | `4.1.3-contexts-class-assertion-forms.hs` |  |
| 4.1.4 | `4.1.4-semantics-overloading-resolution.hs`<br>`4.1.4-semantics-principal-types.hs` |  |
| 4.2.1 | `4.2.1-data-constructors-as-functions.hs`<br>`4.2.1-data-datatype-contexts.hs`<br>`4.2.1-data-empty-declarations.hs`<br>`4.2.1-data-infix-constructors.hs`<br>`4.2.1-data-labelled-fields.hs`<br>`4.2.1-data-mutually-recursive.hs`<br>`4.2.1-data-nullary-and-multi-constructor.hs`<br>`4.2.1-data-parameterised-recursive.hs`<br>`4.2.1-data-shared-field-labels.hs`<br>`4.2.1-data-strictness-flags.hs` |  |
| 4.2.2 | `4.2.2-type-synonyms-in-declarations.hs`<br>`4.2.2-type-synonyms-parameterised-nested.hs`<br>`4.2.2-type-synonyms-recursive-through-data.hs` |  |
| 4.2.3 | `4.2.3-newtype-field-label-and-deriving.hs`<br>`4.2.3-newtype-instances.hs`<br>`4.2.3-newtype-semantics-pattern-match.hs` |  |
| 4.3.1 | `4.3.1-class-constructor-class.hs`<br>`4.3.1-class-default-methods.hs`<br>`4.3.1-class-fixity-and-operator-methods.hs`<br>`4.3.1-class-method-extra-constraints.hs`<br>`4.3.1-class-multiple-superclasses.hs`<br>`4.3.1-class-mutual-defaults.hs`<br>`4.3.1-class-no-methods.hs`<br>`4.3.1-class-superclasses.hs` |  |
| 4.3.2 | `4.3.2-instance-data-and-newtype.hs`<br>`4.3.2-instance-function-type.hs`<br>`4.3.2-instance-functor-user-types.hs`<br>`4.3.2-instance-inherited-defaults.hs`<br>`4.3.2-instance-list-maybe-tuple.hs`<br>`4.3.2-instance-prelude-enum-bounded.hs`<br>`4.3.2-instance-prelude-eq-ord.hs`<br>`4.3.2-instance-prelude-num.hs`<br>`4.3.2-instance-prelude-show.hs` |  |
| 4.3.3 | `4.3.3-deriving-clause-forms.hs` |  |
| 4.3.4 | `4.3.4-defaulting-constraint-combinations.hs`<br>`4.3.4-defaulting-declaration-order.hs`<br>`4.3.4-defaulting-default-default.hs`<br>`4.3.4-defaulting-default-double.hs`<br>`4.3.4-defaulting-default-int.hs`<br>`4.3.4-defaulting-default-rational.hs` |  |
| 4.4.1 | `4.4.1-signatures-local.hs`<br>`4.4.1-signatures-more-specific-than-inferred.hs`<br>`4.4.1-signatures-polymorphic-recursion.hs`<br>`4.4.1-signatures-several-names-and-operators.hs` |  |
| 4.4.2 | `4.4.2-fixity-all-levels.hs`<br>`4.4.2-fixity-backquoted-and-constructors.hs`<br>`4.4.2-fixity-default-infixl9.hs`<br>`4.4.2-fixity-local-declarations.hs` |  |

### `05-modules/` (32 accept, 29 reject)

| Report section | Accept tests | Reject tests (`reject/05-modules/`) |
|---|---|---|
| 5 | `5-default-declaration-is-per-module/` |  |
| 5.1 | `5.1-hierarchical-module-names/`<br>`5.1-main-module-forms/` | `5.1-main-module-without-main/`<br>`5.1-main-not-io/`<br>`5.1-module-name-lowercase/` |
| 5.2 | `5.2-abstract-datatype/`<br>`5.2-class-hierarchy-across-modules/`<br>`5.2-export-duplicates-and-overlaps/`<br>`5.2-export-everything-without-list/`<br>`5.2-export-list-entity-forms/`<br>`5.2-export-qualified-names/`<br>`5.2-fixity-exported-with-operator/`<br>`5.2-records-across-modules/`<br>`5.2-reexport-module-partial-import/`<br>`5.2-reexport-whole-module/`<br>`5.2-types-and-synonyms-across-modules/` | `5.2-abstract-type-constructor-used/`<br>`5.2-export-constructor-of-wrong-type/`<br>`5.2-export-module-not-imported/`<br>`5.2-export-name-clash/`<br>`5.2-export-undefined-name/`<br>`5.2-unexported-class-method-in-instance/`<br>`5.2-unexported-class-method-used/`<br>`5.2-unexported-constructor-used/`<br>`5.2-use-of-unexported-name-qualified/`<br>`5.2-use-of-unexported-name/` |
| 5.3 | `5.3-import-same-module-several-times/`<br>`5.3-imports-are-not-transitive/` | `5.3-import-after-declaration/`<br>`5.3-import-nonexistent-module/`<br>`5.3-qualifier-of-unimported-module/` |
| 5.3.1 | `5.3.1-import-hiding-nonexistent-name/`<br>`5.3.1-import-hiding/`<br>`5.3.1-import-item-lists/` | `5.3.1-hidden-name-used/`<br>`5.3.1-import-constructor-not-exported/`<br>`5.3.1-import-name-not-exported/`<br>`5.3.1-name-not-in-import-list-used/`<br>`5.3.1-type-only-import-constructor-used/` |
| 5.3.2 | `5.3.2-import-qualified/` | `5.3.2-qualified-import-used-unqualified/` |
| 5.3.3 | `5.3.3-import-as-alias/` | `5.3.3-as-alias-original-qualifier-used/` |
| 5.4 | `5.4-instances-always-exported/`<br>`5.4-instances-via-empty-import/` | `5.4-duplicate-instance-across-modules/`<br>`5.4-instance-not-in-scope/` |
| 5.5.1 | `5.5.1-qualified-names-in-declarations/` | `5.5.1-qualified-name-in-binding/` |
| 5.5.2 | `5.5.2-name-clash-resolved-by-qualification/`<br>`5.5.2-same-entity-via-two-paths/` | `5.5.2-ambiguous-import-used-unqualified/`<br>`5.5.2-local-and-import-clash-used-unqualified/` |
| 5.5.3 | `5.5.3-closure-types-not-exported/` |  |
| 5.6 |  | `5.6-prelude-hidden-name-used/` |
| 5.6.1 | `5.6.1-prelude-empty-import-list/`<br>`5.6.1-prelude-explicit-import-hiding/` |  |
| 5.7 | `5.7-mutually-recursive-modules-functions-ghc-differs/`<br>`5.7-mutually-recursive-modules-three-cycle-ghc-differs/`<br>`5.7-mutually-recursive-modules-types-ghc-differs/` |  |

### `06-predefined/` (64 accept, 0 reject)

| Report section | Accept tests | Reject tests (`reject/06-predefined/`) |
|---|---|---|
| 6.1.1 | `6.1.1-bool-laziness.hs`<br>`6.1.1-bool-operators.hs` |  |
| 6.1.2 | `6.1.2-char-ascii-names-and-control.hs`<br>`6.1.2-char-numeric-escapes.hs`<br>`6.1.2-char-simple-escapes.hs`<br>`6.1.2-char-string-show.hs`<br>`6.1.2-string-is-list-of-char.hs` |  |
| 6.1.3 | `6.1.3-lists.hs` |  |
| 6.1.4 | `6.1.4-tuples-functions.hs`<br>`6.1.4-tuples-instances.hs`<br>`6.1.4-tuples-size-15.hs` |  |
| 6.1.5 | `6.1.5-unit.hs` |  |
| 6.1.6 | `6.1.6-function-types.hs` |  |
| 6.1.7 | `6.1.7-io-and-ioerror-types.hs` |  |
| 6.1.8 | `6.1.8-either.hs`<br>`6.1.8-maybe.hs`<br>`6.1.8-ordering.hs` |  |
| 6.2 | `6.2-seq.hs`<br>`6.2-strict-application.hs` |  |
| 6.3.1 | `6.3.1-eq-default-methods.hs`<br>`6.3.1-eq-floating-point.hs`<br>`6.3.1-eq-standard-instances.hs` |  |
| 6.3.2 | `6.3.2-ord-defaults-from-compare.hs`<br>`6.3.2-ord-defaults-from-lte.hs`<br>`6.3.2-ord-max-min-tie-breaking.hs`<br>`6.3.2-ord-standard-instances.hs` |  |
| 6.3.3 | `6.3.3-read-standard-types.hs`<br>`6.3.3-read-user-readsPrec.hs`<br>`6.3.3-read-whitespace-and-parentheses.hs`<br>`6.3.3-reads-failure.hs`<br>`6.3.3-show-functions.hs`<br>`6.3.3-show-negative-double-nested-ghc-differs.hs`<br>`6.3.3-show-negative-numbers-nested.hs`<br>`6.3.3-show-showList.hs`<br>`6.3.3-show-user-showsPrec.hs` |  |
| 6.3.4 | `6.3.4-enum-char-bool-ordering-unit.hs`<br>`6.3.4-enum-double.hs`<br>`6.3.4-enum-float-rational.hs`<br>`6.3.4-enum-int-integer.hs`<br>`6.3.4-enum-user-instances.hs` |  |
| 6.3.5 | `6.3.5-functor-standard-instances.hs`<br>`6.3.5-functor-user-instances.hs` |  |
| 6.3.6 | `6.3.6-monad-fail.hs`<br>`6.3.6-monad-io.hs`<br>`6.3.6-monad-laws.hs`<br>`6.3.6-monad-maybe-list.hs` |  |
| 6.3.7 | `6.3.7-bounded.hs` |  |
| 6.4.1 | `6.4.1-numeric-literals.hs` |  |
| 6.4.2 | `6.4.2-div-mod-quot-rem-int.hs`<br>`6.4.2-div-mod-quot-rem-integer.hs`<br>`6.4.2-even-odd-gcd-lcm.hs`<br>`6.4.2-gcd-zero-zero-ghc-differs.hs`<br>`6.4.2-int-wraparound.hs`<br>`6.4.2-integer-arbitrary-precision.hs` |  |
| 6.4.3 | `6.4.3-exponentiation.hs`<br>`6.4.3-logarithms-sqrt-exp.hs` |  |
| 6.4.4 | `6.4.4-magnitude-and-sign.hs` |  |
| 6.4.5 | `6.4.5-trigonometric-functions.hs` |  |
| 6.4.6 | `6.4.6-coercions-fromIntegral-realToFrac.hs`<br>`6.4.6-properFraction.hs`<br>`6.4.6-realfloat-double.hs`<br>`6.4.6-realfloat-float.hs`<br>`6.4.6-realfloat-ieee-predicates-double.hs`<br>`6.4.6-truncate-round-ceiling-floor.hs` |  |

### `07-io/` (0 accept, 0 reject)

| Report section | Accept tests | Reject tests (`reject/07-io/`) |
|---|---|---|

### `08-ffi/` (0 accept, 0 reject)

| Report section | Accept tests | Reject tests (`reject/08-ffi/`) |
|---|---|---|

### `09-prelude/` (66 accept, 0 reject)

| Report section | Accept tests | Reject tests (`reject/09-prelude/`) |
|---|---|---|
| 9 | `9-class-bounded.hs`<br>`9-class-enum-char-bool-ordering-unit.hs`<br>`9-class-enum-double-float-rational.hs`<br>`9-class-enum-int-integer.hs`<br>`9-class-eq-methods.hs`<br>`9-class-floating-exact.hs`<br>`9-class-floating-tolerance.hs`<br>`9-class-fractional.hs`<br>`9-class-functor-fmap.hs`<br>`9-class-integral-quot-rem-div-mod.hs`<br>`9-class-monad-io.hs`<br>`9-class-monad-maybe-list.hs`<br>`9-class-num-float-double-rational.hs`<br>`9-class-num-int-integer.hs`<br>`9-class-ord-methods.hs`<br>`9-class-real-toRational.hs`<br>`9-class-realfloat-double.hs`<br>`9-class-realfloat-float.hs`<br>`9-class-realfloat-predicates-atan2.hs`<br>`9-class-realfrac-double.hs`<br>`9-class-realfrac-rational-float.hs`<br>`9-fixity-arithmetic-operators.hs`<br>`9-fixity-function-monad-operators.hs`<br>`9-fixity-list-comparison-logic-operators.hs`<br>`9-func-bool-and-or-not-otherwise.hs`<br>`9-func-combinators-id-const-compose-flip-apply.hs`<br>`9-func-maybe-either.hs`<br>`9-func-numeric-fromIntegral-realToFrac.hs`<br>`9-func-numeric-gcd-zero-zero-ghc-differs.hs`<br>`9-func-numeric-power-hat-hathat.hs`<br>`9-func-numeric-subtract-even-odd-gcd-lcm.hs`<br>`9-func-seq-strict-apply-error-undefined.hs`<br>`9-func-tuples-fst-snd-curry-uncurry.hs`<br>`9-func-until-asTypeOf.hs`<br>`9-io-getChar-getLine.hs`<br>`9-io-getContents.hs`<br>`9-io-putChar-putStr-putStrLn-print.hs`<br>`9-list-elem-notElem-lookup.hs`<br>`9-list-foldl-foldl1-foldr-foldr1.hs`<br>`9-list-foldr-laziness-short-circuit.hs`<br>`9-list-head-last-tail-init-null-length-index.hs`<br>`9-list-iterate-repeat-replicate-cycle.hs`<br>`9-list-lines-unlines.hs`<br>`9-list-map-append-filter-concat-concatMap.hs`<br>`9-list-reverse-and-or-any-all.hs`<br>`9-list-scanl-scanl1-scanr-scanr1.hs`<br>`9-list-sum-product-maximum-minimum.hs`<br>`9-list-take-drop-splitAt.hs`<br>`9-list-takeWhile-dropWhile-span-break.hs`<br>`9-list-words-unwords.hs`<br>`9-list-zip-zipWith.hs`<br>`9-list-zip3-zipWith3-unzip-unzip3.hs`<br>`9-monad-mapM-sequence-bind.hs`<br>`9-text-lex.hs`<br>`9-text-read-many-types.hs`<br>`9-text-read-user-instance-readsPrec.hs`<br>`9-text-readList-showList-class-methods.hs`<br>`9-text-readParen.hs`<br>`9-text-reads-readsPrec.hs`<br>`9-text-show-basic-types.hs`<br>`9-text-show-char-string-escapes.hs`<br>`9-text-show-user-instance-showList.hs`<br>`9-text-shows-showChar-showString-showParen.hs`<br>`9-text-showsPrec-negative-numbers.hs`<br>`9-types-bool-maybe-either-ordering.hs`<br>`9-types-char-string-numeric-list-tuple-unit.hs` |  |

### `10-syntax/` (36 accept, 32 reject)

| Report section | Accept tests | Reject tests (`reject/10-syntax/`) |
|---|---|---|
| 2.2 | `2.2-maximal-munch-no-whitespace.hs`<br>`2.2-unicode-in-literals-and-names.hs` |  |
| 2.3 | `2.3-comments-line.hs`<br>`2.3-comments-nested.hs` | `2.3-unterminated-nested-comment.hs` |
| 2.4 | `2.4-identifiers.hs`<br>`2.4-operator-symbols.hs`<br>`2.4-qualified-names-lexing.hs`<br>`2.4-reserved-ops-in-context.hs` | `2.4-consym-as-function-name.hs`<br>`2.4-reserved-op-as-operator.hs`<br>`2.4-reserved-word-as-identifier.hs`<br>`2.4-uppercase-variable.hs` |
| 2.5 | `2.5-numeric-literals-float.hs`<br>`2.5-numeric-literals-integer.hs` | `2.5-float-literal-without-leading-digit.hs`<br>`2.5-hex-literal-without-digits.hs` |
| 2.6 | `2.6-char-literals.hs`<br>`2.6-string-gaps.hs`<br>`2.6-string-literals-escapes.hs` | `2.6-char-escape-out-of-range.hs`<br>`2.6-char-literal-empty.hs`<br>`2.6-char-literal-escape-ampersand.hs`<br>`2.6-string-literal-newline-without-gap.hs`<br>`2.6-string-unknown-escape.hs`<br>`2.6-string-unterminated.hs` |
| 2.7 | `2.7-layout-basic-blocks.hs`<br>`2.7-layout-do-let-and-guards.hs`<br>`2.7-layout-empty-blocks.hs`<br>`2.7-layout-explicit-braces.hs`<br>`2.7-layout-if-then-else-in-do.hs`<br>`2.7-layout-indented-top-level.hs`<br>`2.7-layout-mixed-explicit-implicit.hs`<br>`2.7-layout-no-module-header.hs`<br>`2.7-layout-operators-at-line-start.hs`<br>`2.7-layout-parse-error-rule.hs`<br>`2.7-layout-tabs-are-8-columns.hs`<br>`2.7-layout-where-scoping-and-nesting.hs` | `2.7-layout-do-body-not-indented.hs`<br>`2.7-layout-explicit-brace-unclosed.hs`<br>`2.7-layout-implicit-block-closed-by-explicit-brace.hs`<br>`2.7-layout-item-indented-less.hs`<br>`2.7-layout-where-body-at-top-level-column.hs` |
| 10.4 | `10.4-literate-bird-tracks.lhs`<br>`10.4-literate-latex-code-blocks.lhs` | `10.4-literate-code-adjacent-to-text.lhs` |
| 10.5 | `10.5-declaration-syntax-forms.hs`<br>`10.5-expression-extent-meta-rule.hs`<br>`10.5-type-syntax-forms.hs` | `10.5-do-last-statement-is-bind.hs`<br>`10.5-empty-case.hs`<br>`10.5-if-without-else.hs`<br>`10.5-lambda-without-patterns.hs`<br>`10.5-missing-rhs.hs`<br>`10.5-unbalanced-parenthesis.hs` |
| 10.6 | `10.6-fixity-local-and-shadowed.hs`<br>`10.6-fixity-resolution-prelude-operators.hs`<br>`10.6-fixity-resolution-sections.hs`<br>`10.6-fixity-resolution-unary-minus.hs`<br>`10.6-fixity-resolution-user-operators.hs` | `10.6-fixity-precedence-out-of-range.hs`<br>`10.6-illegal-left-section.hs`<br>`10.6-illegal-right-section.hs`<br>`10.6-mixed-assoc-same-precedence.hs`<br>`10.6-negation-after-tighter-operator.hs`<br>`10.6-negation-with-nonassoc-level-6.hs`<br>`10.6-nonassoc-operators-chained.hs` |
| 12 | `12-pragmas-are-comments.hs` |  |

### `11-deriving/` (0 accept, 0 reject)

| Report section | Accept tests | Reject tests (`reject/11-deriving/`) |
|---|---|---|

### `lib-Control.Monad/` (11 accept, 0 reject)

| Report section | Accept tests | Reject tests (`reject/lib-Control.Monad/`) |
|---|---|---|
| Control.Monad | `filterM-mapAndUnzipM-zipWithM.hs`<br>`foldM-replicateM.hs`<br>`forever-maybe-list-ioerror.hs`<br>`forever-stdin-eof.hs`<br>`functor-fmap.hs`<br>`kleisli-bind-flipped-join-void.hs`<br>`liftM-liftM2-to-liftM5-ap.hs`<br>`mapM-forM-sequence.hs`<br>`monad-bind-then-return-fail.hs`<br>`monadplus-mzero-mplus-msum-guard.hs`<br>`when-unless.hs` |  |

### `lib-Data.Array/` (10 accept, 0 reject)

| Report section | Accept tests | Reject tests (`reject/lib-Data.Array/`) |
|---|---|---|
| Data.Array | `accumArray-accum.hs`<br>`construct-array-listArray.hs`<br>`derived-arrays-ixmap-fmap.hs`<br>`empty-arrays.hs`<br>`incremental-update.hs`<br>`index-types-char-bool-integer.hs`<br>`instances-eq-ord.hs`<br>`instances-show-read.hs`<br>`non-strict-elements-recursive-arrays.hs`<br>`two-dimensional-tuple-indices.hs` |  |

### `lib-Data.Bits/` (8 accept, 0 reject)

| Report section | Accept tests | Reject tests (`reject/lib-Data.Bits/`) |
|---|---|---|
| Data.Bits | `bit-setBit-clearBit-complementBit-testBit.hs`<br>`bitSize-ghc-differs.hs`<br>`fixed-width-signed.hs`<br>`fixed-width-unsigned.hs`<br>`integer-unbounded.hs`<br>`logical-and-or-xor-complement.hs`<br>`rotate-rotateL-rotateR.hs`<br>`shift-shiftL-shiftR.hs` |  |

### `lib-Data.Char/` (9 accept, 0 reject)

| Report section | Accept tests | Reject tests (`reject/lib-Data.Char/`) |
|---|---|---|
| Data.Char | `GeneralCategory-instances.hs`<br>`case-conversion-toUpper-toLower-toTitle.hs`<br>`digitToInt-intToDigit-ord-chr.hs`<br>`generalCategory.hs`<br>`lexLitChar-readLitChar.hs`<br>`predicates-ascii-letters-digits-space.hs`<br>`predicates-ascii-punctuation-symbols.hs`<br>`predicates-latin1.hs`<br>`showLitChar.hs` |  |

### `lib-Data.Complex/` (6 accept, 0 reject)

| Report section | Accept tests | Reject tests (`reject/lib-Data.Complex/`) |
|---|---|---|
| Data.Complex | `construct-realPart-imagPart-conjugate.hs`<br>`instances-eq-show-read.hs`<br>`instances-floating.hs`<br>`instances-fractional.hs`<br>`instances-num.hs`<br>`polar-mkPolar-cis-polar-magnitude-phase.hs` |  |

### `lib-Data.Int/` (5 accept, 0 reject)

| Report section | Accept tests | Reject tests (`reject/lib-Data.Int/`) |
|---|---|---|
| Data.Int | `bounds-wraparound-arithmetic.hs`<br>`conversions-fromIntegral.hs`<br>`instances-eq-ord-enum-show-read.hs`<br>`instances-ix-bits-real.hs`<br>`integral-div-mod-quot-rem.hs` |  |

### `lib-Data.Word/` (5 accept, 0 reject)

| Report section | Accept tests | Reject tests (`reject/lib-Data.Word/`) |
|---|---|---|
| Data.Word | `bounds-unsigned-wraparound.hs`<br>`conversions-fromIntegral.hs`<br>`instances-eq-ord-enum-show-read.hs`<br>`instances-ix-bits-real.hs`<br>`integral-div-mod-quot-rem.hs` |  |
