# Upstream Bug Tracker Scan Process

Before each release, review recently-closed issues and PRs in upstream libraries.
Port regression tests even if kindlings doesn't currently have the bug.

## Upstream Libraries by Module

| Module | Upstream Library | Issue Tracker |
|--------|-----------------|---------------|
| circe-derivation | circe / circe-generic | github.com/circe/circe |
| jsoniter-derivation | jsoniter-scala | github.com/plokhotnyuk/jsoniter-scala |
| cats-derivation | kittens / cats | github.com/typelevel/kittens, github.com/typelevel/cats |
| avro-derivation | avro4s | github.com/sksamuel/avro4s |
| pureconfig-derivation | pureconfig | github.com/pureconfig/pureconfig |
| tapir-schema-derivation | tapir | github.com/softwaremill/tapir |
| scalacheck-derivation | scalacheck-shapeless | github.com/alexarchambault/scalacheck-shapeless |
| diff-derivation | diffx | github.com/softwaremill/diffx |
| yaml-derivation | scala-yaml | github.com/VirtusLab/scala-yaml |
| sconfig-derivation | sconfig | github.com/ekrich/sconfig |

## Checklist

For each upstream library:

1. Check issues closed since last kindlings release
2. For each bug fix:
   - Does kindlings have the same code path?
   - Write a failing test reproducing the upstream bug
   - Verify whether kindlings passes or fails
   - If fails: port the fix
   - If passes: keep the test as regression protection
3. For each new feature:
   - Evaluate whether it belongs in GAPS.md
