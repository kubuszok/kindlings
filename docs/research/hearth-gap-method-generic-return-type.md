# Hearth gap: the return type of a generic method is unknown after `ApplyTypes` (and `Nothing` on Scala 2)

Status: **RESOLVED by kubuszok/hearth#389** (Hearth 0.4.4; Kindlings pins `0.4.3-17-gbd1e277-SNAPSHOT`). `knownReturning`
is known right after `ApplyTypes` and the Scala 2 `Result` is no longer `Nothing`. `ResourceWiringMacrosImpl.returnsResourceOf`
now reads `applied.knownReturning` directly (no placeholder trial-application), and the `ResourceMethodSpec` test below is
a plain test again, passing on 2.13 and 3.

Originally: **OPEN** (found 2026-10-25 with Hearth 0.4.3, while moving `di-cats`' companion `resource[F]` support from
per-platform reflection onto the `Method` builder chain - the follow-up to kubuszok/hearth#331).

## What is wrong

For a companion method like

```scala
object Widget  { def resource[F[_]]: Resource[F, Widget] = ??? }
object Service { def resource[F[_]: Sync](config: Config): Resource[F, Service] = ??? }
```

walking `Class[Widget.type].method("resource")` → `OnInstance.apply(companion)` → `ApplyTypes.apply(F := SyncIO)`:

| step | Scala 2.13 | Scala 3 |
|---|---|---|
| `OnInstance.knownReturning` | `None` | `None` |
| `ApplyTypes.knownReturning` | `None` | `None` |
| after `ApplyTypes` → `ApplyValues.knownReturning` (`Service`) | `None` | `None` |
| after `ApplyTypes` → `Result.knownReturning` (nullary `Widget`) | **`Some(scala.Nothing)`** | `Some(Resource[SyncIO, Widget])` |
| after applying all values → `Result.knownReturning` (`Service`) | **`Some(scala.Nothing)`** | `Some(Resource[SyncIO, Service])` |

Two problems:

1. **Gap (both platforms):** once the type arguments are applied, the return type is fully determined, yet
   `knownReturning` stays `None` until the last value clause is applied. #331 substitutes applied type arguments into
   the *parameter* types of later clauses, but not into the return type. A caller that wants to pick an overload by
   result type has to trial-apply every clause with placeholder arguments.
2. **Bug (Scala 2):** the terminal `Result` of a generic method reports `Returned = Nothing`. `Nothing` conforms to
   everything, so any `<:<` check on it passes.

Observed clause grouping (no bug, but worth documenting): on Scala 2, `expectations` after `ApplyTypes` holds a
**single** `NeedsValues(List(ListMap(config), ListMap(evidence$2)))` for both clauses, and still lists the already
applied `NeedsInstance`/`NeedsTypes` steps. Treat each `ListMap` as one clause.

## Reproducer

`di-cats/src/test/scala/hearth/kindlings/dicats/ResourceMethodSpec.scala`, test "ignores a companion resource[F] that
does not return a Resource of the type":

```scala
final class Plain() { def id: Int = 7 }
object Plain { def resource[F[_]]: Option[Plain] = None }

DICats.wireResource[SyncIO, Plain]() // should build `Plain` through its constructor
```

Scala 3 passes. Scala 2 fails to compile (`found: Resource[SyncIO, Plain], required: Option[Plain]`): the `Nothing`
result type passes the `<:< Resource[F, Plain]` check, so `Plain.resource` gets used.

## Wanted

- `knownReturning` with the applied type arguments substituted, available as soon as `ApplyTypes` has been applied,
  on both platforms.
- `Result.Returned` equal to the real (substituted) return type on Scala 2.

When this is fixed, `ResourceWiringMacrosImpl.returnsResourceOf` can read `applied.knownReturning` directly instead of
trial-applying placeholder arguments.
