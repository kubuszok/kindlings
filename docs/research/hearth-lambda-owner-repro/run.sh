#!/usr/bin/env bash
# Reproducer for docs/research/hearth-gap-lambda-owner-in-val-splice.md (Scala 3, Hearth 0.4.4-10-ga030def-SNAPSHOT).
# The macro and its usage are compiled separately (a Scala 3 macro cannot be expanded in the run that compiles it).
set -euo pipefail
cd "$(dirname "$0")"
scala-cli --power package macro --library -f -o macro.jar
# Expected: the expansions of builtInsideQuotedLambda, valDefsNextToQuotedValsInsideQuotedLambda,
# quotedLambdaInsideLambdaBuilder and lambdaBuilderInsideQuotedLambda fail with
# "assertion failed: Block contains definition with different owners"; builtOutsideQuotedLambda compiles.
scala-cli compile usage
