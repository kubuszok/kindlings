# DI for Cats-Effect `Resource`

`kindlings-di-cats` adds one macro on top of the [DI module](di.md): `DICats.wireResource`, which wires a value whose
dependencies are [Cats-Effect](https://typelevel.org/cats-effect/) `Resource`s (or effects, or plain values) and hands
you back a single composed `Resource`.

It is the Kindlings answer to macwire's `autowire`, with one important difference: **it is not tied to any particular
effect type**. macwire's `autowire` always returns `Resource[IO, _]`; `wireResource` is parametric in `F[_]`, so it
works for `IO`, `SyncIO`, or any other effect — and needs **no** `Sync`/`Async`/`MonadCancel` constraint, because
`Resource`'s `pure`/`eval`/`flatMap` require none.

## Installation

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %%% "kindlings-di-cats" % "{{ kindlings_version() }}"
    ```

!!! example "Scala CLI"

    ```scala
    //> using dep com.kubuszok::kindlings-di-cats::{{ kindlings_version() }}
    ```

`kindlings-di-cats` is published for the JVM and Scala.js. (Scala Native support follows the Cats-Effect Native
artifact, first published from cats-effect 3.7.0.)

## Quick start

Give `wireResource` the dependencies it should compose; it resolves each parameter of the target's constructor from
them and threads the `Resource` plumbing for you:

```scala
//> using dep com.kubuszok::kindlings-di-cats:{{ kindlings_version() }}
//> using dep org.typelevel::cats-effect:3.6.3

import hearth.kindlings.dicats.DICats
import cats.effect.{Resource, SyncIO}

class Db
class Service(db: Db)

val dbResource: Resource[SyncIO, Db] = Resource.eval(SyncIO(new Db))

// db is taken from the Resource and threaded through; the result is a single Resource[SyncIO, Service]:
val app: Resource[SyncIO, Service] = DICats.wireResource[SyncIO, Service](dbResource)

println(app.use(service => SyncIO(service.getClass.getSimpleName)).unsafeRunSync())
// expected output:
// Service
```

## What can be a dependency

Each argument to `wireResource` is classified by its type:

| Dependency type     | How it is used                                                                  |
|---------------------|---------------------------------------------------------------------------------|
| `Resource[F, X]`    | acquired with `flatMap`; its `X` is available to the construction               |
| `F[X]` (an effect)  | lifted with `Resource.eval`, then `flatMap`ed                                   |
| plain `X`           | used directly — spliced into the construction with no `Resource` wrapping       |

The target itself is built from its **public primary constructor** (or, failing that, a single matching companion
`apply`). Acquisition happens in the order the dependencies are given and release happens in reverse, exactly as if you
had written the nested `flatMap`s by hand.

```scala
import cats.effect.{Resource, SyncIO}

class Config
class Db
class Cache
class App(config: Config, db: Db, cache: Cache)

val config = new Config                                     // plain value
val db: Resource[SyncIO, Db] = Resource.eval(SyncIO(new Db))
val cache: SyncIO[Cache] = SyncIO(new Cache)                // effect

val app: Resource[SyncIO, App] = DICats.wireResource[SyncIO, App](config, db, cache)
```

## Errors

`wireResource` mirrors macwire's diagnostics. If no dependency provides a constructor parameter you get a
*"Cannot find a value of type …"* compile error; if two dependencies could provide the same parameter you get an
*"Ambiguous …"* error.

## Relationship to `kindlings-di`

Use [`kindlings-di`](di.md) (`wire`, `wireSet`, …) when you wire plain values from the enclosing lexical scope. Reach
for `kindlings-di-cats` when the components are lifecycle-managed `Resource`s that must be acquired and released
together.
