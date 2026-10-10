#!/bin/bash
# Run the conformance suite.
#
#   conformance/run.sh [-j N] [--write-expected] [DIR ...]
#
# A test is a file NAME.hs or NAME.lhs, or a directory holding Main.hs and
# the modules it imports. What it should print is what GHC makes it print
# under -XHaskell2010, unless the directory holds expected-output.txt (for
# what GHC does not do as the Report says). NAME.in is its input, and it is
# given the arguments: one "two words". A name ending in -exitN leaves
# with status N. Under reject/ a test is one a compiler must refuse.
#
# POWDER is the command that runs a program (default: the built powder-ski),
# and EXPECTED is where what GHC printed is kept between runs.
#
# Each test is one line: PASS, FAIL (Powder differs), or TESTBAD (GHC does
# not do what the test supposes, so the test proves nothing).
cd "$(dirname "$0")/.." || exit 1
JOBS=8
[ "$1" = "-j" ] && { JOBS=$2; shift 2; }
# --write-expected: keep what GHC printed beside each test, where the
# test suite looks for it.
export WRITE=""; [ "$1" = "--write-expected" ] && { WRITE=yes; shift; }
export POWDER=${POWDER:-$(cabal list-bin powder-ski 2>/dev/null)}
export EXPECTED=${EXPECTED:-/tmp/powder-conformance-expected}
mkdir -p "$EXPECTED"

one() {
  t=$1
  if [ -d "$t" ]; then main=$t/Main.hs; dir=$t; else main=$t; dir=$(dirname "$t"); fi
  name=${t%.hs}; name=${name%.lhs}
  input=/dev/null; [ -f "$name.in" ] && input=$PWD/$name.in; [ -d "$t" ] && [ -f "$t/Main.in" ] && input=$PWD/$t/Main.in
  key=$EXPECTED/$(echo "$t" | tr '/ ' '__')
  work=$(mktemp -d)
  case "$t" in
    conformance/reject/*)
      if [ ! -f "$key.ghc" ]; then
        if (cd "$dir" && ghc -XHaskell2010 -fno-code -outputdir "$work" "$(basename "$main")" >/dev/null 2>&1); then echo accepts > "$key.ghc"; else echo refuses > "$key.ghc"; fi
      fi
      # As far as Core, which is as far as anything is refused.
      if timeout 60 $POWDER --dump-core "$main" >/dev/null 2>&1; then powder=accepts; else powder=refuses; fi
      if [ "$(cat "$key.ghc")" = accepts ]; then echo "TESTBAD $t :: GHC accepts it (Powder $powder)"
      elif [ $powder = refuses ]; then echo "PASS $t"
      else echo "FAIL $t :: Powder accepts what must be refused"; fi
      ;;
    *)
      want=0; case "$name" in *-exit[0-9]*) want=${name##*-exit};; esac
      if [ -d "$t" ] && [ -f "$t/expected-output.txt" ]; then cp "$t/expected-output.txt" "$key.out"; echo $want > "$key.status"
      # A file named for GHC differing from the Report has beside it what the
      # Report says it prints, written by hand.
      elif [ "${name%-ghc-differs}" != "$name" ] && [ -f "$name.out" ]; then cp "$name.out" "$key.out"; echo $want > "$key.status"
      elif [ ! -f "$key.out" ]; then
        (cd "$dir" && timeout 120 runghc -XHaskell2010 "$(basename "$main")" one "two words" < "$input" > "$key.out" 2> "$key.err"; echo $? > "$key.status")
      fi
      if [ -n "$WRITE" ] && [ "$(cat "$key.status")" = "$want" ] && [ ! -f "$t/expected-output.txt" ] && [ "${name%-ghc-differs}" = "$name" ]; then
        if [ -d "$t" ]; then cp "$key.out" "$t/Main.out"; else cp "$key.out" "$name.out"; fi
      fi
      if [ "$(cat "$key.status")" != "$want" ]; then echo "TESTBAD $t :: under GHC it leaves with $(cat "$key.status"): $(grep -m1 -E 'error|rror:' "$key.err" 2>/dev/null | cut -c1-140)"; rm -rf "$work"; return; fi
      timeout 60 $POWDER "$main" one "two words" < "$input" > "$work/out" 2> "$work/err"; status=$?
      if [ $status = "$want" ] && cmp -s "$work/out" "$key.out"; then echo "PASS $t"
      else echo "FAIL $t :: exit $status :: $(grep -v '^ *$' "$work/err" | grep -m2 -E 'error\]|╰|runtime error|error:' | cut -c1-150 | tr '\n' ' ')$(diff "$work/out" "$key.out" 2>/dev/null | sed -n '2p;3p' | cut -c1-120 | tr '\n' ' ')"; fi
      ;;
  esac
  rm -rf "$work"
}
export -f one

roots=("$@"); [ ${#roots[@]} = 0 ] && roots=(conformance)
{
  # Under 05-modules a test is a directory; everywhere else, a file.
  for d in conformance/05-modules/*/ conformance/reject/05-modules/*/; do [ -d "$d" ] && echo "${d%/}"; done
  find conformance \( -name '*.hs' -o -name '*.lhs' \) -not -path 'conformance/05-modules/*' -not -path 'conformance/reject/05-modules/*'
} | while read -r t; do
  for r in "${roots[@]}"; do case "$t" in "${r%/}"|"${r%/}"/*) echo "$t"; break;; esac; done
done | sort -u | xargs -P "$JOBS" -I{} bash -c 'one "$1"' _ {} | sort > "$EXPECTED/results.txt"
grep -c '^PASS' "$EXPECTED/results.txt" | sed 's/^/passed: /'
grep -c '^FAIL' "$EXPECTED/results.txt" | sed 's/^/failed: /'
grep -c '^TESTBAD' "$EXPECTED/results.txt" | sed 's/^/tests that prove nothing: /'
