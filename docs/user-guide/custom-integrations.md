# Custom Integrations

Kindlings handles [Cats](cats-integration.md), [Refined](refined-integration.md), [Iron](iron-integration.md),
[neotype](neotype-integration.md) and [scala-newtype](newtype-integration.md) types in **every** derivation module
through small integration modules. You can write such a module for your own types too: a validated identifier, a
wrapper from a newtype library we don't support, a custom collection, an `Option`-like or `Either`-like type. One
module then makes circe, Jsoniter, Avro, YAML, XML, PureConfig, Tapir, Cats, FastShowPretty and every other Kindlings
module (and any other [Hearth](https://scala-hearth.readthedocs.io/)-based macro) handle that type, with no imports and
no implicits at the use site.

This page shows a complete, minimal module for a validated wrapper type.

## Do you need one?

| Your type | What to do |
|---|---|
| `final case class X(value: T) extends AnyVal` | Nothing. It is coded as `T`. Decoding calls the constructor, so a validating smart constructor (e.g. `X.from`) is **not** used |
| Scala 3 `opaque type X = T` whose companion has a method `T => X`, `T => Either[String, X]` or `T => Either[Iterable[String], X]` | Nothing. It is coded as `T`, and decoding runs that method |
| A type from Cats, Refined, Iron, neotype or scala-newtype | Add the matching integration module (links above) |
| Your type, used with one or two libraries | Write the library's instances (e.g. `Encoder`/`Decoder`) in the type's companion object. Kindlings always uses an implicit it finds instead of deriving one |
| Your wrapper or collection, used with many libraries or in many projects | Write an integration module (this page) |
| A case class or sealed hierarchy | Nothing. Kindlings derives it |

!!! example "Opaque types with a smart constructor are supported out of the box"

    ```scala
    //> using scala {{ scala.3 }}
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
    //> using dep io.circe::circe-parser:{{ libraries.circe }}

    import hearth.kindlings.circederivation.{KindlingsDecoder, KindlingsEncoder}
    import io.circe.{Decoder, Encoder}

    object ids {
      opaque type OrderId = Long
      object OrderId {
        def from(value: Long): Either[String, OrderId] =
          if (value > 0) Right(value) else Left(s"OrderId must be positive, got $value")
      }
    }
    import ids.OrderId

    case class Order(id: OrderId, quantity: Int)
    object Order {
      given Encoder[Order] = KindlingsEncoder.derived
      given Decoder[Order] = KindlingsDecoder.derived
    }

    val order = io.circe.parser.decode[Order]("""{"id":3,"quantity":2}""")
    println(order.map(Encoder[Order].apply(_).noSpaces))
    // expected output:
    // Right({"id":3,"quantity":2})
    println(io.circe.parser.decode[Order]("""{"id":-3,"quantity":2}"""))
    // expected output:
    // Left(DecodingFailure at .id: OrderId must be positive, got -3)
    ```

## How it works

Kindlings does not hard-code `Option`, `List` or value classes. It asks Hearth's
[standard extensions](https://scala-hearth.readthedocs.io/en/latest/standard-extensions/) whether a type is a
wrapper (`IsValueType`), a collection (`IsCollection`), a map (`IsMap`), an option (`IsOption`) or an either
(`IsEither`), and how to read and build it. The answers come from **providers**, which the compiler loads with
Java's `ServiceLoader` from the compile classpath while it expands the macro. An integration module is a jar with one
or more providers and a `META-INF/services/hearth.std.StandardMacroExtension` file listing them.

Pick the shape that matches your type:

| Your type looks like | Provider | Decoding |
|---|---|---|
| one wrapped value (id, newtype, refined type) | `IsValueType`: `unwrap` and `wrap` | `wrap` is `CtorLikeOf.PlainValue` (cannot fail) or `EitherStringOrValue` / `EitherIterableStringOrValue` / `EitherThrowableOrValue` / `EitherIterableThrowableOrValue` (validated; the `Left` becomes the format's decoding error) |
| a collection | `IsCollection` | building may fail (e.g. "must not be empty") |
| a map | `IsMap` (registered through `IsCollection`) | as above |
| an optional value | `IsOption` | |
| a two-sided result | `IsEither` | |

The rest of this page builds an `IsValueType` provider. Hearth's
[Standard Extensions](https://scala-hearth.readthedocs.io/en/latest/standard-extensions/) and
[Macro Extensions](https://scala-hearth.readthedocs.io/en/latest/basic-utilities/#macro-extensions) pages describe all
shapes, and the [Kindlings integration modules](#reference-implementations) show collections, maps and eithers.

## Example: a validated identifier

The type lives in your own library, which knows nothing about Kindlings:

```scala
package mylib

final class UserId private (val value: Long) {
  override def equals(other: Any): Boolean = other match {
    case that: UserId => that.value == value
    case _            => false
  }
  override def hashCode: Int = value.hashCode
  override def toString: String = s"UserId($value)"
}
object UserId {
  def from(value: Long): Either[String, UserId] =
    if (value > 0L) Right(new UserId(value)) else Left(s"UserId must be positive, got $value")
}
```

It is not a case class and not an `AnyVal`, so without an integration every derivation fails with:

```
The type mylib.UserId was not handled by any encoder derivation rule:
   ...
   - The rule handle as value type when possible was not applicable, for the following reasons: The type mylib.UserId is not a value type
   ...
```

### The build

The provider must be compiled **before** the code that derives instances, so it goes into its own module. It depends on
[Hearth](https://scala-hearth.readthedocs.io/) (the version that your Kindlings version uses: Kindlings
{{ kindlings_version() }} uses Hearth 0.4.5), not on Kindlings. On Scala 3 it needs Hearth's
[Cross Quotes](https://scala-hearth.readthedocs.io/en/latest/cross-quotes/) compiler plugin, on Scala 2.13
`-Xsource:3` and kind-projector.

```scala
// build.sbt
val hearthVersion    = "0.4.5"
val kindlingsVersion = "{{ kindlings_version() }}"

ThisBuild / scalaVersion       := "{{ scala.3 }}"
ThisBuild / crossScalaVersions := Seq("{{ scala.3 }}", "{{ scala.2_13 }}")

// your types
lazy val mylib = project

// the integration
lazy val mylibKindlings = project
  .in(file("mylib-kindlings"))
  .dependsOn(mylib)
  .settings(
    libraryDependencies += "com.kubuszok" %% "hearth" % hearthVersion,
    libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _)) => Seq(compilerPlugin("com.kubuszok" %% "hearth-cross-quotes" % hearthVersion))
      case _            => Seq(compilerPlugin("org.typelevel" % "kind-projector" % "0.13.4" cross CrossVersion.full))
    }),
    scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _)) => Seq("-Xkind-projector")
      case _            => Seq("-Xsource:3")
    }),
    // tests derive instances with a real Kindlings module
    libraryDependencies ++= Seq(
      "com.kubuszok"  %% "kindlings-circe-derivation" % kindlingsVersion % Test,
      "io.circe"      %% "circe-parser"               % "{{ libraries.circe }}" % Test,
      "org.scalameta" %% "munit"                      % "1.2.1"  % Test
    )
  )

// a project using it
lazy val app = project
  .dependsOn(mylib, mylibKindlings % Provided)
  .settings(
    libraryDependencies ++= Seq(
      "com.kubuszok" %% "kindlings-circe-derivation"    % kindlingsVersion,
      "com.kubuszok" %% "kindlings-jsoniter-derivation" % kindlingsVersion
    )
  )
```

If your users build for Scala.js or Scala Native, publish the integration for those platforms too (they add it with
`%%%`). Macros run in the compiler on the JVM either way.

### The provider

```scala
// mylib-kindlings/src/main/scala/mylib/kindlings/internal/compiletime/IsValueTypeProviderForUserId.scala
package mylib.kindlings.internal.compiletime

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.std.{ProviderResult, StandardMacroExtension, StdExtensions}

final class IsValueTypeProviderForUserId extends StandardMacroExtension { loader =>

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsValueType.registerProvider(new IsValueType.Provider {

      override def name: String = loader.getClass.getName

      private lazy val UserIdType: Type[mylib.UserId] = Type.of[mylib.UserId]
      private lazy val LongType: Type[Long] = Type.of[Long]

      override def parse[A](tpe: Type[A]): ProviderResult[IsValueType[A]] =
        if (tpe =:= UserIdType) {
          implicit val A: Type[A] = tpe
          implicit val L: Type[Long] = LongType

          // how to build a UserId from a decoded Long: it can fail
          val ctor = CtorLikeOf.EitherStringOrValue[Long, A](
            (value: Expr[Long]) =>
              Expr.quote(mylib.UserId.from(Expr.splice(value)).asInstanceOf[Either[String, A]]),
            None
          )
          ProviderResult.Matched(
            Existential[IsValueTypeOf[A, *], Long](new IsValueTypeOf[A, Long] {
              // how to get the Long out of a UserId
              override val unwrap: Expr[A] => Expr[Long] =
                (outer: Expr[A]) => Expr.quote(Expr.splice(outer).asInstanceOf[mylib.UserId].value)
              override val wrap: CtorLikeOf[Long, A] = ctor
              override lazy val ctors: CtorLikes[A] = NonEmptyList.one(Existential[CtorLikeOf[*, A], Long](ctor))
            })
          )
        } else skippedLazily(s"${tpe.prettyPrint} is not mylib.UserId")
    })
  }
}
```

A few rules:

- Return `Matched` only for types you can handle, and decline everything else with `skippedLazily(reason)`. The
  reason shows up in error messages.
- For a wrapper that cannot fail, use `CtorLikeOf.PlainValue[Long, A](value => Expr.quote(...), None)`.
- For a generic wrapper (`Wrapper[T]`), match it with `Type.Ctor1.of[Wrapper].unapply(tpe)` and see the
  [Refined provider](#reference-implementations) for how to work with the extracted type.
- If an instance is needed to build the value (like Refined's `Validate`), summon it in `parse` with
  `Expr.summonImplicit` and decline when it is missing.

### Registration

List the provider class in a services file, one fully-qualified class name per line:

```
# mylib-kindlings/src/main/resources/META-INF/services/hearth.std.StandardMacroExtension
mylib.kindlings.internal.compiletime.IsValueTypeProviderForUserId
```

That's all. Projects that have `mylib-kindlings` on their compile classpath now derive instances for types containing
`UserId` without any import:

```scala
import hearth.kindlings.circederivation.{KindlingsDecoder, KindlingsEncoder}
import hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.core._
import io.circe.{Decoder, Encoder}
import mylib.UserId

case class User(id: UserId, name: String)
object User {
  implicit val encoder: Encoder[User] = KindlingsEncoder.derived[User]
  implicit val decoder: Decoder[User] = KindlingsDecoder.derived[User]
  implicit val codec: JsonValueCodec[User] = KindlingsJsonValueCodec.derived[User]
}

val user = User(UserId.from(42L).toOption.get, "Alice")
println(Encoder[User].apply(user).noSpaces)                              // {"id":42,"name":"Alice"}
println(io.circe.parser.decode[User]("""{"id":-1,"name":"Alice"}"""))
// Left(DecodingFailure at .id: UserId must be positive, got -1)
println(readFromString[User]("""{"id":7,"name":"Bob"}"""))               // User(UserId(7),Bob)
readFromString[User]("""{"id":0,"name":"Bob"}""")
// throws JsonReaderException: UserId must be positive, got 0, offset: 0x00000006, ...
```

Collections, options and map keys of `UserId` work as well: `Team(owners: Map[UserId, String], members: List[UserId],
lead: Option[UserId])` is encoded as `{"owners":{"1":"x"},"members":[1],"lead":1}`.

!!! note
    The examples on this page were verified with sbt and Scala CLI on Scala {{ scala.2_13 }} and {{ scala.3 }}, but
    they are not run by our documentation tests, because the provider has to be published (or be a separate module)
    before it can be used.

## Checking that Kindlings uses it

Turn on [debug logging](debugging.md) for a module, e.g. `-Xmacro-settings:circeDerivation.logDerivation=true`. The
log lists the loaded extensions, and the steps for your type:

```
hearth.std.StandardMacroExtension - Successfully loaded 36 extensions:
  - mylib.kindlings.internal.compiletime.IsValueTypeProviderForUserId
  ...
Attempting to handle mylib.UserId as a value type
...
Derived encoder for mylib.UserId: encode_Long(userid.asInstanceOf[mylib.UserId].value, configuration)
```

(In {{ kindlings_version() }} the list of extensions is printed in the logs of decoders and Jsoniter codecs, but not
in the logs of circe encoders.)

If your type is still "not a value type", the integration is not on the compile classpath of the module where the
instance is derived (e.g. it was added only `% Test`, or only to a downstream module), or the services file is missing
or misspelled.

## Precedence

- **Instances that you provide win.** Kindlings looks for an implicit instance of the type class first, and uses the
  provider only when there is none. With `implicit val userIdEncoder: Encoder[UserId]` in scope, that encoder is used.
- **Between providers, the higher `priority` wins.** Providers have priority `0` by default. Hearth's built-in provider
  for `AnyVal`s also has `0`, and the one for Scala 3 opaque types `-1000`. Kindlings' integrations use `1000`.
  Override `def priority: Int` when your provider must win over another one, e.g. to validate an `AnyVal` with its
  smart constructor (otherwise the order depends on the classpath).
- Types handled by providers are not structural derivations, so a [derivation policy](derivation-policy.md) of
  `opt-in` does not block them.
- [Tapir schema derivation](tapir-schema-derivation.md) gives the type the underlying type's schema without
  validators. Provide a `Schema` yourself to document constraints.

## Testing

Test sources are compiled after main sources, so the integration module can test its own provider (see `Test`
dependencies in the build above):

```scala
// mylib-kindlings/src/test/scala/mylib/kindlings/UserIdProviderSpec.scala
package mylib.kindlings

import hearth.kindlings.circederivation.{KindlingsDecoder, KindlingsEncoder}
import io.circe.{Decoder, Encoder}
import mylib.UserId

final class UserIdProviderSpec extends munit.FunSuite {
  case class User(id: UserId, name: String)
  implicit val encoder: Encoder[User] = KindlingsEncoder.derived[User]
  implicit val decoder: Decoder[User] = KindlingsDecoder.derived[User]

  test("encodes UserId as its underlying Long") {
    assertEquals(encoder(User(UserId.from(42L).toOption.get, "Alice")).noSpaces, """{"id":42,"name":"Alice"}""")
  }
  test("validates on decoding") {
    val result = io.circe.parser.decode[User]("""{"id":0,"name":"Alice"}""")
    assert(result.left.exists(_.getMessage.contains("UserId must be positive")), result)
  }
  test("round trip") {
    val user = User(UserId.from(7L).toOption.get, "Bob")
    assertEquals(io.circe.parser.decode[User](encoder(user).noSpaces), Right(user))
  }
}
```

Run it on both Scala versions (`sbt +mylibKindlings/test`), and add tests for every Kindlings module you care about.

!!! warning "ScalaCheck and validated wrappers"
    In {{ kindlings_version() }}, `Arbitrary` derivation does not support value types whose constructor can fail
    (`EitherStringOrValue` and the other `Either...` variants; this includes Refined types and Scala 3 opaque types with
    an `Either` smart constructor): the generated values fail with a `ClassCastException` at runtime. Provide an
    `Arbitrary` for such a type yourself.

## Runtime footprint

A provider runs only inside the compiler. The generated code calls your library (`UserId.from`, `.value`), not the
provider, so:

- applications can depend on the integration with `% Provided` (as `app` above; in Scala CLI
  `//> using compileOnly.dep`), which keeps it out of the runtime classpath and fat JARs. Every module that derives
  instances needs it on its own compile classpath, though.
- If the generated code has to call a helper at runtime (e.g. a conversion that cannot be written inside a Scala 2
  quote, like the [Cats integration](cats-integration.md) does), keep that helper in a separate package (e.g.
  `internal.runtime`), keep it binary compatible, and let users depend on the module normally.
- Otherwise the same rules as for Kindlings' own modules apply: see
  [Which parts of Kindlings end up in my application at runtime?](faq.md#which-parts-of-kindlings-end-up-in-my-application-at-runtime-can-i-remove-the-rest-from-a-fat-jar).

## Reference implementations

The Kindlings integration modules (at {{ kindlings_version() }}) are complete examples:

- [Refined](https://github.com/kubuszok/kindlings/blob/master/refined-integration/src/main/scala/hearth/kindlings/refinedintegration/internal/compiletime/IsValueTypeProviderForRefined.scala) - `IsValueType` for a type with two type parameters, summoning an instance, validated construction
- [Iron](https://github.com/kubuszok/kindlings/blob/master/iron-integration/src/main/scala/hearth/kindlings/ironintegration/internal/compiletime/IsValueTypeProviderForIron.scala) and [neotype](https://github.com/kubuszok/kindlings/blob/master/neotype-integration/src/main/scala/hearth/kindlings/neotypeintegration/internal/compiletime/IsValueTypeProviderForNeotype.scala) - Scala 3 only
- [scala-newtype](https://github.com/kubuszok/kindlings/blob/master/newtype-integration/src/main/scala/hearth/kindlings/newtypeintegration/internal/compiletime/IsValueTypeProviderForNewtype.scala) - wrappers declared as type members of an object
- Cats: [collections and maps](https://github.com/kubuszok/kindlings/blob/master/cats-integration/src/main/scala/hearth/kindlings/catsintegration/internal/compiletime/CatsCollectionAndMapProviders.scala) with a [runtime helper](https://github.com/kubuszok/kindlings/blob/master/cats-integration/src/main/scala/hearth/kindlings/catsintegration/internal/runtime/CatsConversions.scala), [`Validated` as an either](https://github.com/kubuszok/kindlings/blob/master/cats-integration/src/main/scala/hearth/kindlings/catsintegration/internal/compiletime/IsEitherProviderForValidated.scala), [`Const` as a value type](https://github.com/kubuszok/kindlings/blob/master/cats-integration/src/main/scala/hearth/kindlings/catsintegration/internal/compiletime/IsValueTypeProviderForConst.scala)

In the Hearth documentation:

- [Standard Extensions](https://scala-hearth.readthedocs.io/en/latest/standard-extensions/) - all the shapes, and
  writing providers
- [Macro Extensions](https://scala-hearth.readthedocs.io/en/latest/basic-utilities/#macro-extensions) - how extensions
  are found and loaded
- [Cross Quotes](https://scala-hearth.readthedocs.io/en/latest/cross-quotes/) - `Expr.quote` / `Expr.splice` on both
  Scala versions
