# Testing Derived Instances

What to test, and how, when your codecs, generators and other instances are derived by Kindlings: golden tests that pin
wire formats, round-trip property tests, readable assertion failures, differential tests during a migration, and
compile-time tests. The examples use [MUnit](https://scalameta.org/munit/); ScalaTest equivalents are at the
[end of the page](#scalatest-equivalents).

## Test dependencies

!!! example "sbt"

    ```scala
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit"                           % "1.3.6"                   % Test,
      "org.scalameta" %% "munit-scalacheck"                % "1.3.1"                   % Test,
      "com.kubuszok"  %% "kindlings-scalacheck-derivation" % "{{ kindlings_version() }}" % Test,
      "com.kubuszok"  %% "kindlings-diff-derivation"       % "{{ kindlings_version() }}" % Test
    )
    ```

    Use `%%%` on Scala.js / Scala Native. With Scala CLI, use `//> using test.dep` for the same coordinates.

[ScalaCheck derivation](scalacheck-derivation.md) gives you `Arbitrary` / `Shrink` for your data model and
[Diff derivation](diff-derivation.md) gives you readable assertion failures. Both are test tools, so add them with
`% Test` (see the [FAQ](faq.md#which-parts-of-kindlings-end-up-in-my-application-at-runtime-can-i-remove-the-rest-from-a-fat-jar)).

The module that derives your production codecs (`kindlings-circe-derivation`, ...) and its integrations
(`kindlings-cats-integration`, `kindlings-refined-integration`, ...) must stay in the `Compile` scope of the project
where the derivation happens. An integration added only with `% Test` is invisible to derivations in `src/main`.

## Why test derived instances at all?

With sanely-automatic derivation every instance is generated from the type plus whatever is in implicit scope at the
derivation site: your hand-written instances, the configuration, integrations on the classpath. That is what makes it
convenient, and it is also how the output can change without anybody touching the codec. One new implicit in the
wrong place is enough:

??? example "An implicit added for something else changes an existing wire format"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}

    import hearth.kindlings.circederivation._
    import io.circe.Encoder
    import io.circe.syntax._

    sealed trait Status
    object Status {
      case object Active extends Status
      final case class Suspended(reason: String) extends Status
    }
    final case class Order(id: Long, status: Status)

    object Before {
      implicit val encoder: Encoder[Order] = KindlingsEncoder.derived[Order]
    }
    object After {
      // someone adds this for another type in the same scope...
      implicit val config: Configuration = Configuration.default.withDiscriminator("type")
      // ...and this encoder changes too, while every test that only round-trips still passes
      implicit val encoder: Encoder[Order] = KindlingsEncoder.derived[Order]
    }

    val order = Order(1L, Status.Suspended("fraud"))
    println(order.asJson(Before.encoder).noSpaces)
    // expected output:
    // {"id":1,"status":{"Suspended":{"reason":"fraud"}}}
    println(order.asJson(After.encoder).noSpaces)
    // expected output:
    // {"id":1,"status":{"reason":"fraud","type":"Suspended"}}
    ```

The same happens when a new instance appears in a companion object, when an integration jar is added, when you
migrate from an upstream derivation library ([Feature Parity](feature-parity.md) lists where defaults differ), and
when you upgrade Kindlings itself (some releases change wire formats on purpose, and the release notes say so). A
semi-automatic API would not prevent any of these. **Golden tests** catch them, whatever caused the change.

The [Derivation Policy](derivation-policy.md) is the complementary tool: it controls *where* instances may be
derived, while golden tests check *what* they produce.

## Golden tests

A golden test compares the serialized form of a fixed value with a checked-in expected output. Write one for every
type that leaves the process: HTTP payloads, events, database documents, configuration files.

The MUnit examples on this page test this model (in `src/main`), with circe codecs derived by Kindlings and one
hand-written leaf instance:

```scala
import hearth.kindlings.circederivation._
import io.circe.{Decoder, Encoder}

final case class Money(cents: Long)
object Money {
  implicit val encoder: Encoder[Money] = Encoder.encodeString.contramap(m => f"${m.cents / 100}.${m.cents % 100}%02d")
  implicit val decoder: Decoder[Money] = Decoder.decodeString.emap { s =>
    s.split('.') match {
      case Array(units, cents) => Right(Money(units.toLong * 100 + cents.toLong))
      case _                   => Left(s"not money: $s")
    }
  }
}

sealed trait Status
object Status {
  case object Active extends Status
  final case class Suspended(reason: String) extends Status
}

final case class Order(id: Long, items: List[String], total: Money, note: Option[String], status: Status)
object Order {
  implicit val encoder: Encoder[Order] = KindlingsEncoder.derived[Order]
  implicit val decoder: Decoder[Order] = KindlingsDecoder.derived[Order]
}
```

!!! note "Kindlings 0.4.0 only: declare test-only models at the top level, not inside the suite class"

    On Scala 3, with Kindlings 0.4.0 (fixed in later versions), deriving an instance for a sealed trait whose case object
    is nested in the companion (`object Status { case object Active extends Status }`), when the whole hierarchy is
    declared *inside a class* such as a test suite, crashes the compiler with
    `assertion failed: missing outer accessor in class MySuite`. Put such types at the top level of the file or in a
    top-level `object`.

```scala
import hearth.kindlings.diffderivation._
import io.circe.parser.decode
import io.circe.syntax._

class OrderGoldenSuite extends munit.FunSuite {

  val order = Order(1L, List("apple"), Money(1050), None, Status.Suspended("fraud"))

  test("Order is encoded as agreed with the clients") {
    assertNoDiff( // MUnit's String comparison, not Kindlings' Diff
      order.asJson.noSpaces,
      """{"id":1,"items":["apple"],"total":"10.50","note":null,"status":{"Suspended":{"reason":"fraud"}}}"""
    )
  }

  test("payloads written by the previous release still decode") {
    val stored = """{"id":1,"items":["apple"],"total":"10.50","status":{"Suspended":{"reason":"fraud"}}}"""
    Diff.assertNoDiff(decode[Order](stored).fold(throw _, identity), order)
  }
}
```

Test both directions. The encoding test pins what you send. The decoding test, run on payloads that older versions
of your service produced, checks that you can still read what is already stored or queued.

For larger payloads keep the golden in a file under `src/test/resources` and let an environment variable rewrite it
when a change is intended:

```scala
def assertGolden(name: String, obtained: String)(implicit loc: munit.Location): Unit =
  sys.env.get("UPDATE_GOLDEN") match {
    case Some(dir) => // e.g. UPDATE_GOLDEN=$PWD/src/test/resources/golden
      val file = java.nio.file.Paths.get(dir, name)
      java.nio.file.Files.createDirectories(file.getParent)
      java.nio.file.Files.write(file, obtained.getBytes("UTF-8"))
    case None =>
      val stream = getClass.getResourceAsStream(s"/golden/$name")
      assert(stream != null, s"missing golden file golden/$name, run with UPDATE_GOLDEN=<resources>/golden")
      val expected = try new String(stream.readAllBytes(), "UTF-8") finally stream.close()
      assertNoDiff(obtained, expected)
  }

test("Order golden file") {
  assertGolden("order.json", order.asJson.spaces2)
}
```

A missing file fails the test instead of being created silently, so CI never "passes" a golden that nobody reviewed.
The rewritten files then show up in the pull request diff, where the format change can be reviewed.

### What to pin per format

| Module | Produce the golden with | Notes |
|--------|------------------------|-------|
| [Circe](circe-derivation.md) | `value.asJson.noSpaces` / `.spaces2` | compare strings to pin field order; `io.circe.Json` equality ignores the order of object fields |
| [Jsoniter](jsoniter-derivation.md) | `writeToString(value)` | Kindlings' defaults differ from `JsonCodecMaker.make` |
| [YAML](yaml-derivation.md) | `KindlingsYamlEncoder.toYamlString(value)` | |
| [XML](xml-derivation.md) | `KindlingsXmlEncoder.toXmlString(value, "element")` | |
| [Avro](avro-derivation.md) | `AvroSchemaFor.schemaOf[A].toString` and `AvroIO.toBinary(value)` (as hex) | pin the schema and the bytes; also check schema compatibility, below |
| [PureConfig](pureconfig-derivation.md) | `ConfigWriter[A].to(value).render(ConfigRenderOptions.concise())` | the default rendering includes comments, use `concise()` |
| [Tapir](tapir-schema-derivation.md) | the generated OpenAPI document | the schema follows your JSON codec configuration |

For Avro, the reader/writer compatibility check from Apache Avro protects you against breaking the consumers of data
that is already written:

```scala
import hearth.kindlings.avroderivation._
import org.apache.avro.{Schema, SchemaCompatibility}

final case class ItemAdded(name: String, qty: Int, note: Option[String] = None)

class AvroGoldenSuite extends munit.FunSuite {
  // the schema of the previous release (checked in, or fetched from the schema registry)
  val v1 = new Schema.Parser().parse(
    """{"type":"record","name":"ItemAdded","namespace":"shop.events","fields":[{"name":"name","type":"string"},{"name":"qty","type":"int"}]}"""
  )

  test("the current schema can read data written with v1") {
    val result = SchemaCompatibility.checkReaderWriterCompatibility(AvroSchemaFor.schemaOf[ItemAdded], v1)
    assertEquals(result.getType, SchemaCompatibility.SchemaCompatibilityType.COMPATIBLE, result.getDescription)
  }

  test("binary encoding") {
    assertEquals(AvroIO.toBinary(ItemAdded("apple", 2)).map("%02x".format(_)).mkString, "0a6170706c650400")
  }
}
```

### Keeping goldens maintainable

- Golden-test the **root** types that cross a boundary, with one or two values each that use every field and every
  subtype of the sealed hierarchies. Nested types are covered through them.
- Build the values by hand. Do not generate them with ScalaCheck, because a random value changes the expected output.
- Include the edge cases you care about: `None`, empty collections, defaults, special characters in strings.
- Pin the exact string or bytes when other systems compare them (signatures, hashes, caches). Otherwise compare the
  parsed form so that only meaningful changes fail the test.
- Avoid `Map`/`Set` with more than four entries in golden values: those are hash-based and their order is not the
  insertion order. Use a `ListMap`, or compare parsed JSON.
- When you cross-build for Scala 2.13 and 3, run the same goldens on both versions.

## Round-trip property tests

A round-trip test checks that `decode(encode(a)) == a` for generated values. Derive the generators with
[ScalaCheck derivation](scalacheck-derivation.md), so they cover every field and subtype of the data model and keep
up with it when it changes:

??? example "Round-trip property"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-scalacheck-derivation:{{ kindlings_version() }}
    //> using dep io.circe::circe-parser:{{ libraries.circe }}

    import hearth.kindlings.circederivation._
    import hearth.kindlings.scalacheckderivation.extensions._
    import io.circe.{Decoder, Encoder}
    import io.circe.parser.decode
    import io.circe.syntax._
    import org.scalacheck.{Arbitrary, Prop, Test}

    final case class Item(name: String, qty: Int)
    final case class Basket(owner: String, items: List[Item], coupon: Option[String])

    implicit val encoder: Encoder[Basket] = KindlingsEncoder.derived[Basket]
    implicit val decoder: Decoder[Basket] = KindlingsDecoder.derived[Basket]
    implicit val arbitrary: Arbitrary[Basket] = Arbitrary.derived[Basket]

    val roundTrip = Prop.forAll { (basket: Basket) =>
      decode[Basket](basket.asJson.noSpaces) == Right(basket)
    }
    println(Test.check(Test.Parameters.default, roundTrip).passed)
    // expected output:
    // true
    ```

In an MUnit suite, use `munit.ScalaCheckSuite` and `property`. Deriving `Shrink` as well makes ScalaCheck report a
minimal failing value:

```scala
import hearth.kindlings.diffderivation._
import hearth.kindlings.scalacheckderivation.extensions._
import io.circe.parser.decode
import io.circe.syntax._
import org.scalacheck.{Arbitrary, Gen, Prop, Shrink}

class OrderRoundTripSuite extends munit.ScalaCheckSuite {
  // a hand-written generator for a type with invariants; derivation uses it for every Money field
  implicit val arbMoney: Arbitrary[Money] = Arbitrary(Gen.choose(0L, 1000000L).map(Money(_)))
  implicit val arbOrder: Arbitrary[Order] = Arbitrary.derived[Order]
  implicit val shrinkOrder: Shrink[Order] = Shrink.derived[Order]

  property("decode(encode(order)) == order") {
    Prop.forAll { (order: Order) =>
      Diff.assertNoDiff(decode[Order](order.asJson.noSpaces).fold(throw _, identity), order)
    }
  }
}
```

Round-trip tests do **not** replace golden tests: an encoder and a decoder that change together (like in the example
at the top of this page) still round-trip.

### When a round-trip legitimately fails

Some values do not survive a round-trip, even though both the encoder and the decoder do what they should:

??? example "Option[Option[A]] and NaN"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-diff-derivation:{{ kindlings_version() }}
    //> using dep io.circe::circe-parser:{{ libraries.circe }}

    import hearth.kindlings.circederivation._
    import hearth.kindlings.diffderivation._
    import io.circe.{Decoder, Encoder}
    import io.circe.parser.decode
    import io.circe.syntax._

    final case class Patch(nickname: Option[Option[String]], score: Double)

    implicit val encoder: Encoder[Patch] = KindlingsEncoder.derived[Patch]
    implicit val decoder: Decoder[Patch] = KindlingsDecoder.derived[Patch]

    val original = Patch(nickname = Some(None), score = Double.NaN)
    val json = original.asJson.noSpaces
    val decoded = decode[Patch](json).fold(throw _, identity)

    println(json)
    // expected output:
    // {"nickname":null,"score":null}

    try Diff.assertNoDiff(decoded, original)
    catch { case e: AssertionError => println(e.getMessage) }
    // expected output:
    // Values differ:
    // Patch(
    //   nickname = None -> Some(None),
    //   score = NaN -> NaN,
    // )
    ```

| Case | What happens | What to do |
|------|--------------|------------|
| `Double.NaN` / infinities | `NaN != NaN`, so even a correct round-trip fails `==`; circe writes `null`, jsoniter throws `illegal number: NaN` while writing | ScalaCheck's default `Arbitrary[Double]` does not produce them, but your own generators and real data can; filter them out of the generator, or compare with a tolerant [`Diff.approximate`](diff-derivation.md) / your own equality |
| `Option[Option[A]]` | `Some(None)` and `None` are both written as `null` (circe), so `Some(None)` comes back as `None` | exclude `Some(None)` from the generator, or use a format that keeps them apart (see [Jsoniter](jsoniter-derivation.md) `withSkipNestedOptionValues`) |
| `None` vs a missing field | both decode to `None`, so the round-trip passes, but the encoded form (`null` or no field) is a wire-format decision | pin it with a golden test |
| Fields with default values | with jsoniter's default config, `C(xs = Nil)` for `xs: List[Int] = List(1)` is written as `[]` and read back as `List(1)` | `withEmptyCollectionsAsEmpty`, see [Jsoniter](jsoniter-derivation.md) |
| `@transientField` | the field is not written and is decoded as its default value | generate only the default for that field, or compare values with the field reset |
| Strings with unpaired surrogates | jsoniter refuses to write them (`illegal char sequence of surrogate pair`) | ScalaCheck's default `Arbitrary[String]` does not produce them; custom generators might |
| `Array` fields | arrays compare by reference, so `==` fails | compare with `Diff.assertNoDiff`, or use `Vector` / `List` in the model |

## Readable failures with Diff

`assertEquals(decoded, expected)` on two large case classes prints two long `toString`s. `Diff.assertNoDiff` from
[Diff derivation](diff-derivation.md) prints only what differs:

```scala
import hearth.kindlings.diffderivation._

Diff.assertNoDiff(obtained, expected) // throws AssertionError("Values differ:\n...") if they differ
val result = Diff.assertDiff(a, b)    // throws if they are identical, returns the DiffResult
```

A failure prints, for example:

```text
Values differ:
Order(
  id = 1,
  items = List(
      "apple",
    + "pear",
  ),
  total = Money(...),
  note = None,
  status = Status.Suspended(...),
)
```

- Changes are shown as `left -> right`, so pass the obtained value first and the expected value second.
- The message is plain text (no ANSI colours), so it is readable in CI logs.
- `Diff` instances are derived on demand. Put `Diff.approximate(epsilon)` for `Double` in implicit scope to tolerate
  rounding, or `Diff.derived[A].ignoreField("updatedAt")` to skip a field.
- Call it as `Diff.assertNoDiff(...)`. MUnit's own `assertNoDiff(obtained: String, expected: String)` is inherited by
  every suite, so after `import Diff._` the unqualified name still refers to MUnit's method and fails with
  `Found: Order, Required: String`.

## Differential tests during a migration

While you replace an upstream derivation library, keep it as a **test** dependency and compare both instances on
generated values. This finds every difference, including the ones that hand-picked golden values do not exercise:

```scala
// libraryDependencies += "io.circe" %% "circe-generic" % "{{ libraries.circe }}" % Test
import hearth.kindlings.scalacheckderivation.extensions._
import io.circe.Encoder
import io.circe.syntax._
import org.scalacheck.{Arbitrary, Gen, Prop}

class DifferentialSuite extends munit.ScalaCheckSuite {
  implicit val arbMoney: Arbitrary[Money] = Arbitrary(Gen.choose(0L, 1000000L).map(Money(_)))
  implicit val arbOrder: Arbitrary[Order] = Arbitrary.derived[Order]

  // what circe-generic used to produce
  val upstream: Encoder[Order] = {
    import io.circe.generic.semiauto._
    implicit val status: Encoder[Status] = deriveEncoder[Status]
    deriveEncoder[Order]
  }

  property("Kindlings writes the same JSON as circe-generic") {
    Prop.forAll { (order: Order) =>
      assertEquals(order.asJson, upstream(order)) // order.asJson uses the Kindlings-derived Order.encoder
    }
  }
}
```

- Keep the upstream instances local (a block or a dedicated object), so they cannot leak into the implicit scope of the
  code under test.
- Test decoders the same way: decode the upstream encoder's output with the Kindlings decoder, and the other way round.
- When a difference is expected (a default that differs, see [Feature Parity](feature-parity.md)), either configure
  Kindlings to match, or assert the exact expected difference rather than skipping the case.
- Remove the upstream dependency and the differential suite when the migration is finished. The golden tests stay.

## Compile-time tests

Some guarantees can only be checked at compile time: that a type does **not** get an instance, that the derivation
policy rejects ad-hoc derivation, or that your hand-written instance is used. MUnit's `compileErrors` type-checks a
code snippet when the test is compiled and returns the error messages as a `String`:

```scala
class CompileTimeSuite extends munit.FunSuite {

  test("a type with a Thread field does not get an Encoder") {
    val errors: String = compileErrors("hearth.kindlings.circederivation.KindlingsEncoder.derived[Option[Thread]]")
    val plain = errors.replaceAll("\u001b\\[[0-9;]*m", "") // type names may contain ANSI colour codes
    assert(plain.contains("java.lang.Thread was not handled by any encoder derivation rule"), plain)
  }

  test("the hand-written Encoder[Money] wins over derivation") {
    val json = Order(1L, Nil, Money(5), None, Status.Active).asJson
    assertEquals(json.hcursor.get[String]("total"), Right("0.05"))
  }
}
```

With the [Derivation Policy](derivation-policy.md) enabled for the test sources, you can check that ad-hoc derivation
outside the allowed scopes is rejected, and that the marker import enables it:

```scala
// scalacOptions:
//   -Xmacro-settings:circeDerivation.policy.enabled=opt-in
//   -Xmacro-settings:circeDerivation.policy.allowedScopes=com.acme.model
test("ad-hoc derivation is rejected outside com.acme.model") {
  val errors: String = compileErrors("hearth.kindlings.circederivation.KindlingsEncoder.derived[com.acme.model.Address]")
  assert(errors.contains("is enabled only in the following scopes"), errors)
}

test("the marker import allows it") {
  val errors: String = compileErrors("""
    import hearth.kindlings.circederivation.policy._
    hearth.kindlings.circederivation.KindlingsEncoder.derived[com.acme.model.Address]
  """)
  assertEquals(errors, "")
}
```

!!! note "Untyped `val`s and the derivation policy in Kindlings 0.4.0"

    In Kindlings 0.4.0, with the derivation policy enabled, `val errors = compileErrors(...)` without a type made the
    derivation fail with `Recursive value errors needs type` (Scala 3) or `illegal cyclic reference involving value
    errors` (Scala 2.13), and on Scala 2.13 so did any untyped `val` holding a derived instance. Later versions read the
    enclosing scope without forcing those types; on 0.4.0, write `val errors: String = compileErrors(...)`.

Match only stable fragments of the message, like the rule name or the policy sentence. The full message lists every
rule that was tried, and the list can grow between versions.

## ScalaTest equivalents

| MUnit | ScalaTest |
|-------|-----------|
| `class S extends munit.ScalaCheckSuite` + `property(...) { Prop.forAll { ... } }` | `class S extends AnyFunSuite with ScalaCheckPropertyChecks` + `forAll { (a: A) => ... }` (`org.scalatestplus::scalacheck-1-18`) |
| `assertNoDiff(obtainedString, expectedString)` | `assert(obtained == expected)`, or `Diff.assertNoDiff` |
| `compileErrors(code)` (returns the message) | `assertTypeError(code)` / `assertDoesNotCompile(code)` (no access to the message) |

`Diff.assertNoDiff` and the derived `Arbitrary` / `Shrink` work the same in ScalaTest. ScalaTest's
`assertDoesNotCompile` also passes on a typo in the snippet, so prefer `assertTypeError`, or MUnit's `compileErrors`
when the message matters.

## CI

Test sources often derive many instances at once (`Arbitrary`, `Shrink`, `Diff` for the whole model, plus the
codecs), and CI machines are slower than laptops. If a derivation that compiles locally hits
`Macro '...' timed out after 5000ms` on CI, raise the [derivation timeout](debugging.md#derivation-timeout) for the
test sources only:

```scala
Test / scalacOptions ++= Seq(
  "-Xmacro-settings:scalacheckDerivation.timeout=30s",
  "-Xmacro-settings:diffDerivation.timeout=30s"
)
```

Run the golden tests on every platform and Scala version you publish for. They are cheap, and they are the tests that
notice when cross-built code behaves differently.
