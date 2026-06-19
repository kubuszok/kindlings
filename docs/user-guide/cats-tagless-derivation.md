# Cats Tagless Derivation

Drop-in replacement for [cats-tagless](https://typelevel.org/cats-tagless/)'s `Derive.*` / `derived` macros — derives `FunctorK`, `ContravariantK`, `InvariantK`, `ApplyK`, `SemigroupalK`, and `Instrument` for tagless-final algebras (both `trait Alg[F[_]]` and `case class Alg[F[_]]`), on Scala 2.13 **and** Scala 3.

## Installation

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %% "kindlings-cats-tagless-derivation" % "{{ kindlings_version() }}"
    ```

    Cross-platform (JVM / Scala.js / Scala Native):

    ```scala
    libraryDependencies += "com.kubuszok" %%% "kindlings-cats-tagless-derivation" % "{{ kindlings_version() }}"
    ```

!!! example "Scala CLI"

    ```scala
    //> using dep com.kubuszok::kindlings-cats-tagless-derivation:{{ kindlings_version() }}
    ```

The `org.typelevel::cats-tagless-core` dependency (which provides `FunctorK`, `InvariantK`, `Instrument`, …) is brought in transitively.

## Quick start

??? example "Deriving FunctorK and transforming an algebra's effect"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-cats-tagless-derivation:{{ kindlings_version() }}

    import hearth.kindlings.catstaglessderivation._
    import cats.tagless.FunctorK
    import cats.arrow.FunctionK

    trait UserRepo[F[_]] {
      def find(id: Int): F[String]
    }

    val functorK: FunctorK[UserRepo] = KindlingsFunctorK.derived

    // an interpreter that runs in Option
    val optionRepo: UserRepo[Option] = new UserRepo[Option] {
      def find(id: Int): Option[String] = if (id == 1) Some("Alice") else None
    }

    // lift the whole algebra from Option into List via a natural transformation
    val toList: FunctionK[Option, List] = new FunctionK[Option, List] {
      def apply[A](fa: Option[A]): List[A] = fa.toList
    }

    val listRepo: UserRepo[List] = functorK.mapK(optionRepo)(toList)

    println(listRepo.find(1))
    // expected output:
    // List(Alice)
    ```

## Derived type classes

| Method | Derives | Use |
|--------|---------|-----|
| `KindlingsFunctorK.derived[Alg]` | `cats.tagless.FunctorK[Alg]` | `mapK` — map an algebra's effect `F ~> G` (covariant occurrences) |
| `KindlingsContravariantK.derived[Alg]` | `cats.tagless.ContravariantK[Alg]` | `contramapK` — for algebras where `F` appears only in contravariant position |
| `KindlingsInvariantK.derived[Alg]` | `cats.tagless.InvariantK[Alg]` | `imapK` — for algebras where `F` appears in both positions |
| `KindlingsApplyK.derived[Alg]` | `cats.tagless.ApplyK[Alg]` | `productK` / `mapK` (extends `FunctorK` + `SemigroupalK`) |
| `KindlingsSemigroupalK.derived[Alg]` | `cats.tagless.SemigroupalK[Alg]` | `productK` — combine two interpreters into one over `Tuple2K` |
| `KindlingsInstrument.derived[Alg]` | `cats.tagless.aop.Instrument[Alg]` | weave aspect-oriented instrumentation around each method |

Each entry point is an `inline def derived` on Scala 3 and a `def derived` macro on Scala 2 — the call shape is identical on both.

## Supported algebra shapes

| Shape | Support |
|-------|---------|
| `trait Alg[F[_]]` with `F[_]`-returning methods (`def get(id: Int): F[String]`) | Yes |
| `case class Alg[F[_]]` with `F[_]` fields (`a: F[Int]`, `b: F[String]`) | Yes |
| Mixed members — some `F[_]`, some plain (`def version: String`) | Yes (plain members preserved, only `F[_]` ones transformed) |
| `F[_]` in argument position (`def transform(in: F[Int]): F[String]`) | Yes via `InvariantK` |
| Nested algebras (`inner: InnerAlg[F]`) | Yes |
| Recursive / nested algebra without an explicit instance for the inner type | Yes (the inner instance is derived recursively) |
| Algebras with no `F[_]` members at all | Yes via `ContravariantK` / `InvariantK` (all members preserved verbatim) |

## Debugging

Enable derivation logging with an import:

```scala
import hearth.kindlings.catstaglessderivation.debug._
```

Or globally via a scalac option:

```
-Xmacro-settings:catsTaglessDerivation.logDerivation=true
```

The macro timeout (default 5 seconds) can be raised per module:

```
-Xmacro-settings:catsTaglessDerivation.timeout=30s
```
