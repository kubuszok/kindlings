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

1. resolving each parameter of `A`'s **public primary constructor** from the enclosing scope — or of the public
   constructor annotated with `@javax.inject.Inject` / `@jakarta.inject.Inject`, if there is one (also a secondary
   constructor; two annotated constructors are an error), as in macwire;
2. if the primary constructor is inaccessible, falling back to a **single matching companion `apply`**;
3. resolving **implicit parameters** (an `implicit`/`using` parameter clause) through normal implicit search rather
   than from the scope. A parameter merely flagged `implicit` in an ordinary clause (`class S(val a: A, implicit val
   b: B)`) is an implicit member, wired from the scope like the others.

Candidates are gathered from:

- the members (declared **and inherited**) of the enclosing classes/objects — so module composition by trait
  inheritance works — with the members declared in the class/object itself ranked before the inherited ones (see
  below);
- the public, parameterless members of any in-scope value whose type is annotated [`@Module`](#modules);
- the **parameters of the enclosing method** (every parameter clause, `implicit`/`using` ones included; by-name
  parameters are evaluated where they are needed), on Scala 2 and Scala 3; and
- (Scala 2 only) `val`s local to the enclosing method declared before the call site.

When the same type is available in more than one scope, the **nearest** (innermost) scope wins; an ambiguity is an
error only when a single scope offers more than one candidate. The enclosing method's parameters (together with its
local `val`s) are the innermost scope, as in macwire: a parameter shadows a member of the same type, while two
parameters of the same type are reported as an ambiguity, with their names.

Within an enclosing class or object the candidates are ranked as in macwire:

1. the members **declared** in the class/object itself (an `override` of an inherited member counts as declared);
2. the **inherited** members (from parent classes/traits and from a self-type), together with the members of a
   [`@Module`](#modules) held by a declared member;
3. the members of a `@Module` held by an inherited member.

So a declared member shadows an inherited member of the same type, while two declared (or two inherited) members of
the same type are an ambiguity:

```scala
trait DatabaseDefaults {
  lazy val defaultDatabaseAccess: DatabaseAccess = new DatabaseAccess()
  lazy val securityFilter: SecurityFilter = new SecurityFilter()
}

class TestModule extends DatabaseDefaults {
  lazy val testDatabaseAccess: DatabaseAccess = new DatabaseAccess()
  // testDatabaseAccess (declared) wins over defaultDatabaseAccess (inherited); securityFilter is inherited
  lazy val userFinder: UserFinder = DI.wire[UserFinder]
}
```

```scala
object Wiring {
  lazy val securityFilter = new SecurityFilter()

  // databaseAccess comes from the parameter, securityFilter from the enclosing object
  def userFinder(databaseAccess: DatabaseAccess): UserFinder = DI.wire[UserFinder]
}
```

Only the **closest** enclosing method counts: a lambda between the call and the method is looked through, but the
parameters of outer methods are not used, and a class body ends the search (`wire` in a local or anonymous class sees
that class's members, not the parameters of the method that defines it).

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

A `classOf[Impl]` dependency tells `autowire` to construct `Impl` wherever one of its supertypes is needed
(`DI.autowire[B](classOf[AImpl])` with `class B(a: A)` and `class AImpl extends A`), as in macwire.

A dependency is a **factory** when its static type is a function type itself — `(A, B) => C`, also through a type
alias — or, on Scala 3, a context function (`(A, B) ?=> C`, whose `using` parameters are wired from the graph like
ordinary ones). Pass a context function as a literal: a reference to a `val f: A ?=> C` is applied by the compiler
before the macro sees it. A value whose class merely *extends* a function type (`List[A]`, `Map[K, V]`,
`class Maker extends (A => B)`) is an ordinary instance. The same rules apply to `DICats.wireResource`.

`autowire` reports, at compile time:

- **cyclic dependencies**,
- attempts to wire a **primitive or `String`**, and
- any provided dependency that ended up **unused**.

### `autowireMembersOf` — a module's members as dependencies

`DI.autowireMembersOf(instance)` (or the bare `autowireMembersOf(instance)` with `import hearth.kindlings.di.*`, the
macwire spelling), passed as an argument of `autowire`, adds every public, parameterless member (`val`,
`lazy val`, no-arg `def`) of `instance` as a dependency, each matched by its declared type (macwire's
`autowireMembersOf`). A module usually has more members than one graph needs, so its unused members are not errors;
only a module none of whose members is used is reported, as `unused dependencies: DI.autowireMembersOf(module)`.
Outside `autowire` the call is the identity.

```scala
class AppModule {
  val db = new DatabaseAccess()
  val sf = new SecurityFilter()
  val metrics = new Metrics() // not needed by UserFinder: fine
}

val module = new AppModule
val finder: UserFinder = DI.autowire[UserFinder](DI.autowireMembersOf(module))
```

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

### `wiredInModule` — a runtime registry

`DI.wiredInModule(module)` captures every public, parameterless member of `module` whose type is a reference type
(primitives are skipped) into a `Wired` registry keyed by the member's declared type, for looking instances up at
runtime by class (macwire's `wiredInModule`):

```scala
import hearth.kindlings.di.{DI, Wired}

val wired: Wired = DI.wiredInModule(new DataModule)

wired.lookup(classOf[DatabaseAccess])             // List(databaseAccess): every entry assignable to the class
wired.lookupSingleOrThrow(classOf[DatabaseAccess]) // the single match; IllegalArgumentException on none or several
wired.withInstances(new SecurityFilter())          // a new Wired with extra instances (keyed by runtime class)
wired.withInstanceFactory(() => new SecurityFilter()) // ... or an extra factory (keyed by its ClassTag)
```

Entries are factories: a `def` member is re-read on each lookup. Lookups are subtype-aware but use erased classes, so
type arguments are not distinguished. macwire's reflective `wireClassInstance` / `wireClassInstanceByName` are not
ported (they need JVM reflection, unavailable on Scala.js and Scala Native).

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
- **Enclosing-method parameters / locals.** The parameters of the closest enclosing method are candidates on both
  Scala 2 and Scala 3 (via Hearth's `Environment.enclosingMethodParameters`, since 0.4.4). macwire also looks at the
  parameters of outer enclosing methods and of enclosing lambdas; Kindlings does not. Local `val`s declared before
  the call site are discovered on Scala 2 only.
- Only the **immediately**-enclosing class exposes a usable `this`; members of *outer* enclosing classes are not
  reachable (enclosing **objects** always are, since they have a stable path).
- **Scala 2: annotate the members holding a `wire`.** On Scala 2.13 `val c = wire[C]` (or `def make(b: B) = wire[D]`)
  without a type fails with `recursive value c needs type`: write `val c: C = wire[C]`. macwire's examples leave the
  type out; Kindlings reads the enclosing scope through Hearth, which forces those types on Scala 2.
- **Not candidates (macwire: candidates):** members brought in by an `import`, constructor parameters of the enclosing
  class that are not `val`s, values bound by a pattern, local `val`s of a nested block (`if` branches), and on Scala 3
  any local `val`.
- **`wireList` order**: the members declared in the class come before the inherited ones (macwire's Scala 3 `wireList`
  test case expects the inherited member first).
- **Error wording** differs in places (same information), e.g. `unused dependencies: C()` (macwire: `C.apply()`),
  `No public primary constructor found for [T]` + `Companion object for [T] has no apply methods ...` (macwire:
  `Cannot find a public constructor ... for [T]`).
- On Scala 2, `wireWith` of a method with a by-name parameter eta-expanded with `_` (`wireWith(Component.create _)`) is
  not supported.

macwire's own test cases (2.6.7) run against Kindlings in `di/src/test/scala*/hearth/kindlings/di/upstream/`; the
cases for the differences above assert the compilation error (or the different result), so a fix shows up there.
