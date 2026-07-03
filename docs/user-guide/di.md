# Dependency Injection (DI)

Compile-time dependency injection — a [macwire](https://github.com/softwaremill/macwire) port built on Hearth's
`enclosingScope`. `wire` and friends inspect the **enclosing lexical scope** at compile time and generate the
constructor calls for you: no reflection, no runtime container, and — unlike macwire — the exact same code
cross-compiles to **JVM, Scala.js and Scala Native** on both Scala 2.13 and Scala 3.

## Installation

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %%% "kindlings-di" % "{{ kindlings_version() }}"
    ```

!!! example "Scala CLI"

    ```scala
    //> using dep com.kubuszok::kindlings-di:{{ kindlings_version() }}
    ```

The [`com.softwaremill.common::tagging`](https://github.com/softwaremill/scala-common) library is pulled in
transitively so the `@@` tag types are available out of the box.

## Quick start

Each `wire[A]` resolves every parameter of `A`'s primary constructor from a value of a compatible type found in the
enclosing class/object, and emits `new A(...)`:

```scala
import hearth.kindlings.di.DI

class DatabaseAccess()
class SecurityFilter()
class UserFinder(databaseAccess: DatabaseAccess, securityFilter: SecurityFilter)

class UserModule {
  lazy val databaseAccess = new DatabaseAccess()
  lazy val securityFilter = new SecurityFilter()
  lazy val userFinder: UserFinder = DI.wire[UserFinder]   // = new UserFinder(databaseAccess, securityFilter)
}
```

Prefer the bare `wire` (the macwire spelling)? Import the package object — both spellings are equivalent:

```scala
import hearth.kindlings.di._

class UserModule {
  lazy val databaseAccess = new DatabaseAccess()
  lazy val securityFilter = new SecurityFilter()
  lazy val userFinder: UserFinder = wire[UserFinder]
}
```

## Resolution rules

`wire` builds an `A` by, in order:

1. resolving each parameter of `A`'s **public primary constructor** from the enclosing scope;
2. if the primary constructor is inaccessible, falling back to a **single matching companion `apply`**;
3. resolving **implicit parameters** through normal implicit search rather than from the scope.

Candidates are gathered from:

- the members (declared **and inherited**) of the enclosing classes/objects — so module composition by trait
  inheritance works;
- the public, parameterless members of any in-scope value whose type is annotated [`@Module`](#modules); and
- (Scala 2 only) `val`s local to the enclosing method declared before the call site.

When the same type is available in more than one scope, the **nearest** (innermost) scope wins; an ambiguity is an
error only when a single scope offers more than one candidate.

### Composition by inheritance

```scala
trait DatabaseModule {
  lazy val databaseAccess: DatabaseAccess = new DatabaseAccess()
  lazy val securityFilter: SecurityFilter = new SecurityFilter()
}

class AppModule extends DatabaseModule {
  lazy val userFinder: UserFinder = DI.wire[UserFinder]   // inherited members are candidates
}
```

## `wireRec` — recursive wiring

`wireRec[A]` is like `wire`, but any dependency **not** found in scope is itself constructed recursively (refusing to
fabricate `java.*`/`scala.*` library types):

```scala
class Service(databaseAccess: DatabaseAccess)
class Handler(databaseAccess: DatabaseAccess)
class App(service: Service, handler: Handler)

class RecursiveModule {
  lazy val databaseAccess = new DatabaseAccess()
  lazy val app: App = DI.wireRec[App]   // Service and Handler are built for you from databaseAccess
}
```

## `wireWith` — factory wiring

`wireWith` resolves the parameters of a factory function (a lambda or an eta-expanded method/constructor reference)
from the scope and applies it. Useful when construction is not a plain constructor call:

```scala
def makeUserFinder(db: DatabaseAccess, sf: SecurityFilter): UserFinder = new UserFinder(db, sf)

class FactoryModule {
  lazy val databaseAccess = new DatabaseAccess()
  lazy val securityFilter = new SecurityFilter()
  lazy val userFinder: UserFinder = DI.wireWith(makeUserFinder _)
  // or: DI.wireWith((db: DatabaseAccess, sf: SecurityFilter) => new UserFinder(db, sf))
}
```

Factory arities 0–22 are supported.

## `wireSet` / `wireList`

Collect **every** value of a type found in scope:

```scala
class Plugin
class PluginModule {
  val a, b, c: Plugin = new Plugin
  lazy val plugins: Set[Plugin]      = DI.wireSet[Plugin]    // Set(a, b, c)
  lazy val pluginList: List[Plugin]  = DI.wireList[Plugin]   // List(a, b, c) — declaration order preserved
}
```

## `autowire` — explicit object graphs

`autowire[A](dependencies*)` builds a complete object graph for `A` from an **explicit** list of dependencies —
instances and/or factory functions — constructing everything else via public constructors / companion `apply`s. Each
distinct type is instantiated exactly once and shared through a generated local `val`:

```scala
val db = new DatabaseAccess()
val sf = new SecurityFilter()

val finder: UserFinder = DI.autowire[UserFinder](db, sf)

// factory functions are accepted too:
val finder2 = DI.autowire[UserFinder](db, sf, (d: DatabaseAccess, s: SecurityFilter) => new UserFinder(d, s))
```

`autowire` reports, at compile time:

- **cyclic dependencies**,
- attempts to wire a **primitive or `String`**, and
- any provided dependency that ended up **unused**.

## Tagging

Disambiguate two values of the same base type with [tags](https://github.com/softwaremill/scala-common) — the `@@`
type combinator. Resolution falls out of subtyping, so no extra wiring annotations are needed:

```scala
import com.softwaremill.tagging._

trait Primary
trait Replica
class Database(url: String)
class Connector(primary: Database @@ Primary, replica: Database @@ Replica)

class TaggedModule {
  val primaryDb: Database @@ Primary = new Database("primary").taggedWith[Primary]
  val replicaDb: Database @@ Replica = new Database("replica").taggedWith[Replica]
  lazy val connector: Connector = DI.wire[Connector]   // each tag is matched independently
}
```

## Modules

Annotate a type with `@Module` to let `wire` reach into a value of that type for its public, parameterless members —
useful for composing applications out of pre-built modules passed as constructor arguments:

```scala
import hearth.kindlings.di.Module

@Module
class DataModule {
  lazy val databaseAccess: DatabaseAccess = new DatabaseAccess()
  lazy val securityFilter: SecurityFilter = new SecurityFilter()
}

class AppModule(dataModule: DataModule) {
  // databaseAccess / securityFilter are pulled out of dataModule because DataModule is a @Module
  lazy val userFinder: UserFinder = DI.wire[UserFinder]
}
```

## `DI.plan` — recursive, cached, customizable wiring (Kindlings' own)

`wire` / `wireRec` / `autowire` follow **macwire's** conventions. `DI.plan[A]` is **Kindlings' own opinionated
endpoint** — it is *always* recursive and *always* caches (each distinct type is built once and shared), and it lets
you customize how the graph is assembled **without touching your domain classes**. Finish the builder chain with
`.build`:

```scala
import hearth.kindlings.di.DI

class DatabaseAccess()
class UserFinder(databaseAccess: DatabaseAccess)
class App(userFinder: UserFinder)

object Main {
  // builds the whole graph from public constructors: App -> UserFinder -> DatabaseAccess
  val app: App = DI.plan[App].build
}
```

### Storage strategy

By default every constructed dependency is stored as a `val`. Change the whole-graph default, or override it per type:

```scala
import hearth.kindlings.di.DI

class DatabaseAccess()
class UserFinder(databaseAccess: DatabaseAccess)
class App(userFinder: UserFinder)

object Main {
  val app: App =
    DI.plan[App]
      .asLazyVals                       // whole-graph default: val | asLazyVals | asDefs
      .storeAsDef[DatabaseAccess]       // per-type override: storeAsVal | storeAsLazyVal | storeAsDef
      .build
}
```

- **`asVals`** (default) / **`storeAsVal[T]`** — created once, eagerly.
- **`asLazyVals`** / **`storeAsLazyVal[T]`** — created once, on first use.
- **`asDefs`** / **`storeAsDef[T]`** — re-created on every use (so a `def`-stored dependency is **not** shared).

### Construction overrides

Tell `plan` to initialise a specific type with your own factory instead of calling its constructor — "when you need a
`T`, use this":

```scala
import hearth.kindlings.di.DI

class DatabaseAccess()
class UserFinder(databaseAccess: DatabaseAccess)
class App(userFinder: UserFinder)

object Main {
  val app: App =
    DI.plan[App]
      .provide[DatabaseAccess](new DatabaseAccess()) // by-name factory, evaluated per the type's storage
      .build
}
```

## Seeing how things are wired — the wiring graph

Inspired by [ZIO Magic / ZIO 2.0's automatic layer wiring](https://zio.dev) (`ZLayer.Debug.tree` /
`ZLayer.Debug.mermaid`), every DI endpoint can print how the entities/resources combine, as a DAG-aware ASCII tree or a
[Mermaid](https://mermaid.live) diagram. For `DI.plan` request it inline with `.debugTree` / `.debugMermaid`:

```scala
import hearth.kindlings.di.DI

class DatabaseAccess()
class RecService(databaseAccess: DatabaseAccess)
class RecHandler(databaseAccess: DatabaseAccess)
class RecApp(service: RecService, handler: RecHandler)

object Main {
  val app: RecApp = DI.plan[RecApp].debugTree.build
}
```

prints (at compile time) the shared `DatabaseAccess` once, referencing it under the second consumer:

```text
RecApp
├─ RecService  [val]
│  ╰─ DatabaseAccess  [val]
╰─ RecHandler  [val]
   ╰─ DatabaseAccess  ↻ (shared, wired above)
```

For **any** endpoint (including `wire`/`autowire`), enable the graph project-wide with a scalac option — `tree` for the
ASCII view, `mermaid` for the Mermaid diagram + a render link:

```scala
scalacOptions += "-Xmacro-settings:di.logWiring=mermaid"
```

To also see the full generation log (the resolution logic **and** the generated code), enable
`di.logGeneration` — see [Debugging & Diagnostics](debugging.md).

## Differences from macwire

- **`DI.plan` is not macwire.** `wire`/`wireRec`/`autowire` mirror macwire; `DI.plan` is Kindlings' own opinionated
  endpoint (always recursive, always caching, customizable storage + construction overrides).
- **Cross-platform.** macwire's request/session *scopes* rely on JVM bytecode proxies; this module targets JVM,
  Scala.js and Scala Native, so those JVM-only proxy scopes are intentionally not ported.
- **Enclosing-method parameters / locals.** Local `val`s declared before the call site are discovered on Scala 2;
  enclosing-method *parameters* are not currently exposed cross-platform by the underlying macro API.
- Only the **immediately**-enclosing class exposes a usable `this`; members of *outer* enclosing classes are not
  reachable (enclosing **objects** always are, since they have a stable path).
