#!/usr/bin/env bash
# Finds deprecated standard-library APIs in the code Kindlings' Scala 3 macros GENERATE.
#
# Quotes are compiled (and pickled) with the build's Scala 3 version, but expanded in users' code, which may be compiled
# with a newer Scala 3 whose standard library deprecates what the quote calls (e.g. Scala 3.10 deprecates
# `Option.orNull`'s implicit-evidence overload and `Predef.ArrowAssoc`). Such warnings appear at every derivation site
# and break users' `-Werror` builds, and the build itself cannot see them.
#
# For each module, this compiles its Scala 3 test sources (which exercise the derivations) with a newer Scala 3 and
# `-deprecation`, against the module's classes compiled by sbt, and reports the deprecation warnings (with the macro
# source line they were inlined from).
#
# Usage (from the repository root, with an sbt server running):
#   scripts/check-generated-code-deprecations.sh [scala-version] [module=dir[=extra-scalac-option] ...]
# e.g.
#   scripts/check-generated-code-deprecations.sh 3.10.0 jsoniterDerivation=jsoniter-derivation \
#     tapirSchemaDerivation=tapir-schema-derivation=-Xmacro-settings:tapirSchemaDerivation.preferConfig=circe
# Requires scala-cli. Exits with 1 if any deprecation warning was found.
set -u
ROOT=$(pwd)
SCALA=${1:-3.10.0}
shift || true
OUT=$(mktemp -d)
CSR=$HOME/.cache/coursier/v1
[ -d "$HOME/Library/Caches/Coursier/v1" ] && CSR=$HOME/Library/Caches/Coursier/v1
found=0
for spec in "$@"; do
  IFS='=' read -r mod dir extra <<< "$spec"
  sbt --client "export $mod/Test/fullClasspath" > "$OUT/cp-raw-$mod.txt" 2>&1
  # sbt 2 prints virtual paths (${OUT}, ${CSR_CACHE}); the build's own Scala library is replaced by the newer one
  grep "^List(" "$OUT/cp-raw-$mod.txt" | head -1 | sed 's/^List(//; s/)$//' | tr ',' '\n' | sed 's/^ *//; s/>.*//' |
    sed "s#\${OUT}#$ROOT/target/out#; s#\${CSR_CACHE}#$CSR#" |
    grep -v "SBT_BOOT\|scala3-library\|scala-library\|-tests.jar" > "$OUT/cp-$mod.txt"
  CP=$(paste -sd: "$OUT/cp-$mod.txt")
  dirs=()
  for d in scala scala-3 scalajvm scalajvm-3 java; do [ -d "$ROOT/$dir/src/test/$d" ] && dirs+=("$ROOT/$dir/src/test/$d"); done
  opts=(-deprecation -source:3.3)
  [ -n "${extra:-}" ] && opts+=("$extra")
  (cd "$OUT" && scala-cli compile --server=false --scala "$SCALA" "${opts[@]}" --classpath "$CP" "${dirs[@]}" \
    > "$OUT/compile-$mod.txt" 2>&1)
  sed 's/\x1b\[[0-9;]*m//g' "$OUT/compile-$mod.txt" > "$OUT/compile-$mod-plain.txt"
  count=$(grep -c 'Deprecation Warning' "$OUT/compile-$mod-plain.txt")
  echo "== $mod: $count deprecation warning(s) $(grep 'errors\? found' "$OUT/compile-$mod-plain.txt")"
  grep "is deprecated\|inlined from" "$OUT/compile-$mod-plain.txt" | sed 's/^ *|//' | sort | uniq -c | sort -rn
  [ "$count" -gt 0 ] && found=1
done
echo "(compiler output in $OUT)"
exit $found
