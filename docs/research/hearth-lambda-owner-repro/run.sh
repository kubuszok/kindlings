#!/usr/bin/env bash
# Reproducer for docs/research/hearth-gap-lambda-owner-in-val-splice.md (Scala 3, Hearth 0.4.4-13-g9038396-SNAPSHOT).
# The macro and its usage are compiled separately (a Scala 3 macro cannot be expanded in the run that compiles it).
set -euo pipefail
cd "$(dirname "$0")"
scala-cli --power package macro --library -f -o macro.jar
# With Hearth 0.4.4-10 the expansions of builtInsideQuotedLambda, valDefsNextToQuotedValsInsideQuotedLambda,
# quotedLambdaInsideLambdaBuilder and lambdaBuilderInsideQuotedLambda failed with
# "assertion failed: Block contains definition with different owners"; builtOutsideQuotedLambda compiled.
# Hearth d62d007e (0.4.4-11, reverted) fixed those but broke matchCaseChainInsideQuotedLambda (variant M).
# Fixed in Hearth 90383964 (0.4.4-13-g9038396-SNAPSHOT): every variant compiles.
scala-cli compile usage
