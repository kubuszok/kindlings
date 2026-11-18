# HTTP APIs with tapir

A recipe for building an HTTP API with [tapir](https://tapir.softwaremill.com/) where the JSON on the wire, the tapir
`Schema` and the generated OpenAPI document **agree by construction**: one Kindlings JSON configuration drives the
codecs, and the schemas are derived from that same configuration.

The pieces:

| Concern | Module | Page |
|---------|--------|------|
| JSON codecs | `kindlings-jsoniter-derivation` **or** `kindlings-circe-derivation` | [Jsoniter](jsoniter-derivation.md), [Circe](circe-derivation.md) |
| tapir `Schema` | `kindlings-tapir-schema-derivation` | [Tapir Schema](tapir-schema-derivation.md) |
| OpenAPI JSON | `kindlings-tapir-openapi-jsoniter` (or tapir's circe-based docs) | [Tapir OpenAPI Jsoniter](tapir-openapi-jsoniter.md) |
| Constraints as validators | refined / Iron integrations + tapir's own integrations | [Refined](refined-integration.md), [Iron](iron-integration.md) |

## Choosing the JSON library

Both work with tapir's `jsonBody[A]`, which needs a codec **and** a `Schema[A]` for the body type:

| | jsoniter-scala | circe |
|---|---|---|
| Codecs | `kindlings-jsoniter-derivation` | `kindlings-circe-derivation` |
| tapir integration | `tapir-jsoniter-scala` (`import sttp.tapir.json.jsoniter._`) | `tapir-json-circe` (`import sttp.tapir.json.circe._`) |
| Configuration | `JsoniterConfig` | `Configuration` |
| `None` fields | written as `null` unless `withTransientNone` | written as `null` unless the printer drops them |
| circe / cats on the classpath | no | yes |

Pick one library per API. With jsoniter the whole stack (codecs, schemas, OpenAPI rendering) stays free of circe and
cats.

!!! example "sbt (jsoniter)"

    ```scala
    libraryDependencies ++= Seq(
      "com.kubuszok"                %% "kindlings-jsoniter-derivation"     % "{{ kindlings_version() }}",
      "com.kubuszok"                %% "kindlings-tapir-schema-derivation" % "{{ kindlings_version() }}",
      "com.kubuszok"                %% "kindlings-tapir-openapi-jsoniter"  % "{{ kindlings_version() }}",
      "com.softwaremill.sttp.tapir" %% "tapir-jsoniter-scala"              % "{{ libraries.tapir }}"
    )
    ```

!!! example "sbt (circe)"

    ```scala
    libraryDependencies ++= Seq(
      "com.kubuszok"                %% "kindlings-circe-derivation"        % "{{ kindlings_version() }}",
      "com.kubuszok"                %% "kindlings-tapir-schema-derivation" % "{{ kindlings_version() }}",
      "com.kubuszok"                %% "kindlings-tapir-openapi-jsoniter"  % "{{ kindlings_version() }}", // optional
      "com.softwaremill.sttp.tapir" %% "tapir-json-circe"                  % "{{ libraries.tapir }}"
    )
    ```

## The recipe

1. **One configuration per API**, defined once (an `object` you import, or a top-level `given` in Scala 3) and visible
   wherever codecs *and* schemas are derived.
2. **Instances in companions**: for every type used directly as a request or response body, put the codec and the
   schema next to each other in its companion object. Nested types (fields, subtypes) are derived recursively with
   the same configuration, so they need no instances of their own.
3. **Schemas from `KindlingsSchema.derived[A].schema`** - never tapir's `Schema.derived` or
   `sttp.tapir.generic.auto._`, which know nothing about the Kindlings configuration.
4. **Render the documentation** with `TapirOpenApi` (jsoniter, no circe) or tapir's circe-based `openapi-circe`.
5. **Test that they agree**: encode sample values and check them against the published component schemas, and/or
   golden-test the OpenAPI JSON.

## Complete example

An endpoint taking a sealed-trait body, with a discriminator field and `snake_case` names. The JSON printed for the
sample values and the component schemas of the OpenAPI document use the same field names, the same discriminator
field and the same discriminator values:

```scala
//> using scala {{ scala.2_13 }}
//> using dep com.kubuszok::kindlings-jsoniter-derivation:{{ kindlings_version() }}
//> using dep com.kubuszok::kindlings-tapir-schema-derivation:{{ kindlings_version() }}
//> using dep com.kubuszok::kindlings-tapir-openapi-jsoniter:{{ kindlings_version() }}
//> using dep com.softwaremill.sttp.tapir::tapir-jsoniter-scala:{{ libraries.tapir }}

import hearth.kindlings.jsoniterderivation._
import hearth.kindlings.tapirschemaderivation._
import com.github.plokhotnyuk.jsoniter_scala.core._
import sttp.tapir._
import sttp.tapir.json.jsoniter._

// One configuration for the whole API - codecs and schemas both read it.
object ApiConfig {
  implicit val config: JsoniterConfig = JsoniterConfig.default
    .withSnakeCaseFieldNames         // accountIban -> account_iban
    .withSnakeCaseAdtLeafClassNames  // BankTransfer -> bank_transfer
    .withDiscriminator("type")       // {"type":"bank_transfer",...} instead of {"BankTransfer":{...}}
    .withTransientNone               // omit None instead of writing null
}
import ApiConfig._

sealed trait Payment
object Payment {
  implicit val codec: JsonValueCodec[Payment] = KindlingsJsonValueCodec.derived[Payment]
  implicit val schema: Schema[Payment] = KindlingsSchema.derived[Payment].schema
}
case class CardPayment(cardNumber: String, expiryMonth: Int) extends Payment
case class BankTransfer(accountIban: String, reference: Option[String]) extends Payment

case class PaymentId(paymentId: String)
object PaymentId {
  implicit val codec: JsonValueCodec[PaymentId] = KindlingsJsonValueCodec.derived[PaymentId]
  implicit val schema: Schema[PaymentId] = KindlingsSchema.derived[PaymentId].schema
}

val createPayment = endpoint.post
  .in("payments")
  .in(jsonBody[Payment])
  .out(jsonBody[PaymentId])

// What the server reads and writes:
println(writeToString[Payment](CardPayment("4111111111111111", 12)))
// expected output:
// {"type":"card_payment","card_number":"4111111111111111","expiry_month":12}
println(writeToString[Payment](BankTransfer("DE89370400440532013000", None)))
// expected output:
// {"type":"bank_transfer","account_iban":"DE89370400440532013000"}

// What the OpenAPI document says (rendered with jsoniter, no circe involved):
import hearth.kindlings.tapiropenapijsoniter._
import hearth.kindlings.tapiropenapijsoniter.OpenApiJsoniter.openapi_3_1._
import sttp.apispec.openapi.Info

val components = TapirOpenApi.toOpenAPI(createPayment, Info("Payments", "1.0")).components.get.schemas
println(writeToString(components("Payment")))
// expected output:
// {"title":"Payment","oneOf":[{"$ref":"#/components/schemas/BankTransfer"},{"$ref":"#/components/schemas/CardPayment"}],"discriminator":{"propertyName":"type","mapping":{"bank_transfer":"#/components/schemas/BankTransfer","card_payment":"#/components/schemas/CardPayment"}}}
println(writeToString(components("CardPayment")))
// expected output:
// {"title":"CardPayment","type":"object","required":["card_number","expiry_month","type"],"properties":{"card_number":{"type":"string"},"expiry_month":{"type":"integer","format":"int32"},"type":{"type":"string","const":"card_payment"}}}
println(writeToString(components("BankTransfer")))
// expected output:
// {"title":"BankTransfer","type":"object","required":["account_iban","type"],"properties":{"account_iban":{"type":"string"},"reference":{"type":"string"},"type":{"type":"string","const":"bank_transfer"}}}
```

The same code compiles unchanged on Scala 3 and prints the same output. `reference` is optional in the schema because
the decoder accepts it missing; see [required and optional fields](tapir-schema-derivation.md#required-and-optional-fields)
for the full rules. To get the whole document instead of single components, use
`TapirOpenApi.endpointsToJson(endpoints, info)`.

### Scala 3 with `derives`

`derives KindlingsSchema` produces a `KindlingsSchema[A]`, but tapir's `jsonBody` asks for a `Schema[A]`, so add a
`given` that unwraps it. The configuration must be visible where the type is defined (here a top-level `given`):

```scala
//> using scala {{ scala.3 }}
//> using dep com.kubuszok::kindlings-jsoniter-derivation:{{ kindlings_version() }}
//> using dep com.kubuszok::kindlings-tapir-schema-derivation:{{ kindlings_version() }}
//> using dep com.softwaremill.sttp.tapir::tapir-jsoniter-scala:{{ libraries.tapir }}

import hearth.kindlings.jsoniterderivation.*
import hearth.kindlings.tapirschemaderivation.*
import com.github.plokhotnyuk.jsoniter_scala.core.*
import sttp.tapir.*
import sttp.tapir.json.jsoniter.*

given JsoniterConfig = JsoniterConfig.default.withSnakeCaseFieldNames.withDiscriminator("type")

sealed trait Payment derives KindlingsJsonValueCodec, KindlingsSchema
object Payment:
  given Schema[Payment] = summon[KindlingsSchema[Payment]].schema // jsonBody needs a tapir Schema
case class CardPayment(cardNumber: String, expiryMonth: Int) extends Payment
case class BankTransfer(accountIban: String, reference: Option[String]) extends Payment

val createPayment = endpoint.post.in("payments").in(jsonBody[Payment])
println(writeToString[Payment](CardPayment("4111111111111111", 12)))
// expected output:
// {"type":"CardPayment","card_number":"4111111111111111","expiry_month":12}
```

### The same API with circe

With circe codecs, two things change:

- `kindlings-tapir-openapi-jsoniter` depends on `kindlings-jsoniter-derivation`, so **both** JSON configurations are
  visible and the schema macro fails with `Multiple JSON library configurations found: circe, jsoniter-scala.` Put a
  `PreferSchemaConfig[Configuration]` next to the configuration (or use the
  `-Xmacro-settings:tapirSchemaDerivation.preferConfig=circe` scalac option, see
  [disambiguating multiple JSON configs](tapir-schema-derivation.md#disambiguating-multiple-json-configs)).
- circe writes `None` as `null`, while the schema describes an optional (not nullable) field. tapir's circe integration
  prints with its `jsonPrinter`; override it to drop `null` values.

```scala
//> using scala {{ scala.2_13 }}
//> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
//> using dep com.kubuszok::kindlings-tapir-schema-derivation:{{ kindlings_version() }}
//> using dep com.kubuszok::kindlings-tapir-openapi-jsoniter:{{ kindlings_version() }}
//> using dep com.softwaremill.sttp.tapir::tapir-json-circe:{{ libraries.tapir }}

import hearth.kindlings.circederivation._
import hearth.kindlings.tapirschemaderivation._
import io.circe.{Codec, Printer}
import sttp.tapir._
import sttp.tapir.json.circe.TapirJsonCirce

object ApiConfig {
  implicit val config: Configuration = Configuration.default
    .withSnakeCaseMemberNames
    .withSnakeCaseConstructorNames
    .withDiscriminator("type")
  // kindlings-tapir-openapi-jsoniter brings kindlings-jsoniter-derivation (and its JsoniterConfig) along:
  // tell the schema macro to follow circe's configuration.
  implicit val preferCirce: PreferSchemaConfig[Configuration] = PreferSchemaConfig[Configuration]
}
import ApiConfig._

// tapir's circe integration, printing without `"field":null` (the schema says optional, not nullable).
object ApiJson extends TapirJsonCirce {
  override def jsonPrinter: Printer = Printer.noSpaces.copy(dropNullValues = true)
}
import ApiJson._

sealed trait Payment
object Payment {
  implicit val codec: Codec.AsObject[Payment] = KindlingsCodecAsObject.derived[Payment]
  implicit val schema: Schema[Payment] = KindlingsSchema.derived[Payment].schema
}
case class CardPayment(cardNumber: String, expiryMonth: Int) extends Payment
case class BankTransfer(accountIban: String, reference: Option[String]) extends Payment

val body = jsonBody[Payment]
val createPayment = endpoint.post.in("payments").in(body)

// The exact body the endpoint sends:
println(body.codec.encode(BankTransfer("DE89370400440532013000", None)))
// expected output:
// {"account_iban":"DE89370400440532013000","type":"bank_transfer"}

import hearth.kindlings.tapiropenapijsoniter._
import hearth.kindlings.tapiropenapijsoniter.OpenApiJsoniter.openapi_3_1._
import com.github.plokhotnyuk.jsoniter_scala.core._

val components = TapirOpenApi.toOpenAPI(createPayment, "Payments", "1.0").components.get.schemas
println(writeToString(components("BankTransfer")))
// expected output:
// {"title":"BankTransfer","type":"object","required":["account_iban","type"],"properties":{"account_iban":{"type":"string"},"reference":{"type":"string"},"type":{"type":"string","const":"bank_transfer"}}}
```

Without `kindlings-tapir-openapi-jsoniter` (rendering the docs with circe instead, see below) only the circe
configuration is visible and `PreferSchemaConfig` is not needed.

## Where configuration and instances go

- **The configuration is looked up where each instance is derived.** A codec derived in a companion that imports
  `ApiConfig._` and a schema derived somewhere without that import silently use different configurations (the schema
  falls back to `Configuration.default` / `JsoniterConfig.default` from the companion). Keeping both instances in the
  same companion, under the same import, rules that out.
- **One configuration per API.** An instance in a companion exists once. If two APIs need different wire formats for
  the same type, derive both the codec and the schema for the second API in that API's own object (with its own
  configuration in scope), not in the companion.
- **Scala 2**: a configuration stored in an `object` member is read at runtime, which works the same; only some
  compile-time validations of the configuration are skipped. Always give `implicit val`s an explicit type.
- **Scala 3**: a top-level `given` or a `given` in an imported object works for both `derives` and
  `KindlingsSchema.derived`.

## Rendering the OpenAPI document

| | `kindlings-tapir-openapi-jsoniter` | tapir + sttp-apispec circe |
|---|---|---|
| Dependencies | `kindlings-tapir-openapi-jsoniter` (brings `tapir-openapi-docs`) | `tapir-openapi-docs` + `"com.softwaremill.sttp.apispec" %% "openapi-circe"` / `openapi-circe-yaml` |
| JSON | `TapirOpenApi.endpointsToJson(endpoints, info)` | `OpenAPIDocsInterpreter().toOpenAPI(endpoints, info).asJson` (`import sttp.apispec.openapi.circe._`) |
| YAML | - | `.toYaml` (`import sttp.apispec.openapi.circe.yaml._`) |
| Platforms | `TapirOpenApi`: JVM, Scala.js; `OpenApiJsoniter` codecs: also Native | JVM, Scala.js |

Both produce the same JSON (after `deepDropNullValues` on the circe side). Note that tapir's
`tapir-swagger-ui-bundle` depends on `openapi-circe-yaml` and therefore on circe; `tapir-swagger-ui` takes the
document as a string and does not. See [Tapir OpenAPI Jsoniter](tapir-openapi-jsoniter.md) for the OpenAPI 3.0
output and the other options.

## Constraints as validators

The [refined](refined-integration.md) and [Iron](iron-integration.md) integrations make the codecs validate while
decoding, but `KindlingsSchema` documents such fields with the underlying type's schema. Add tapir's integration for
the library (`tapir-refined`, `tapir-iron`) and import its schemas where the schema is derived; Kindlings uses them
like any other implicit `Schema`:

```scala
//> using scala {{ scala.2_13 }}
//> using dep com.kubuszok::kindlings-jsoniter-derivation:{{ kindlings_version() }}
//> using dep com.kubuszok::kindlings-tapir-schema-derivation:{{ kindlings_version() }}
//> using dep com.kubuszok::kindlings-tapir-openapi-jsoniter:{{ kindlings_version() }}
//> using dep com.kubuszok::kindlings-refined-integration:{{ kindlings_version() }}
//> using dep com.softwaremill.sttp.tapir::tapir-jsoniter-scala:{{ libraries.tapir }}
//> using dep com.softwaremill.sttp.tapir::tapir-refined:{{ libraries.tapir }}

import hearth.kindlings.jsoniterderivation._
import hearth.kindlings.tapirschemaderivation._
import hearth.kindlings.tapiropenapijsoniter._
import com.github.plokhotnyuk.jsoniter_scala.core._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive
import sttp.apispec.openapi.Info
import sttp.tapir._
import sttp.tapir.json.jsoniter._

case class OrderLine(sku: String, quantity: Int Refined Positive)
object OrderLine {
  import sttp.tapir.codec.refined._ // validators for refined types, used by KindlingsSchema
  implicit val codec: JsonValueCodec[OrderLine] = KindlingsJsonValueCodec.derived[OrderLine]
  implicit val schema: Schema[OrderLine] = KindlingsSchema.derived[OrderLine].schema
}

val addLine = endpoint.post.in("lines").in(jsonBody[OrderLine])
import OpenApiJsoniter.openapi_3_1._
val schemas = TapirOpenApi.toOpenAPI(addLine, Info("Orders", "1.0")).components.get.schemas
println(writeToString(schemas("OrderLine")))
// expected output:
// {"title":"OrderLine","type":"object","required":["sku","quantity"],"properties":{"sku":{"type":"string"},"quantity":{"type":"integer","format":"int32","exclusiveMinimum":0}}}
println(scala.util.Try(readFromString[OrderLine]("""{"sku":"A-1","quantity":0}""")).failed.get.getMessage.takeWhile(_ != ','))
// expected output:
// Predicate failed: (0 > 0).
```

With Iron (Scala 3 only) add `kindlings-iron-integration` and `tapir-iron` and use `import sttp.tapir.codec.iron.given`.

## Testing that they agree

### Sample values against the component schemas

Encode sample values with the real codec and compare them with the component schemas of the document you publish.
The check below needs nothing beyond the modules above (it parses the JSON with
[`kindlings-jsoniter-json`](jsoniter-json.md), which `kindlings-tapir-openapi-jsoniter` already depends on). This
configuration lacks `withTransientNone`, and the check catches the resulting `null`:

```scala
//> using scala {{ scala.2_13 }}
//> using dep com.kubuszok::kindlings-jsoniter-derivation:{{ kindlings_version() }}
//> using dep com.kubuszok::kindlings-tapir-schema-derivation:{{ kindlings_version() }}
//> using dep com.kubuszok::kindlings-tapir-openapi-jsoniter:{{ kindlings_version() }}
//> using dep com.softwaremill.sttp.tapir::tapir-jsoniter-scala:{{ libraries.tapir }}

import hearth.kindlings.jsoniterderivation._
import hearth.kindlings.tapirschemaderivation._
import hearth.kindlings.tapiropenapijsoniter.TapirOpenApi
import hearth.kindlings.jsoniterjson.Json
import hearth.kindlings.jsoniterjson.codec.JsonCodec.jsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.core._
import sttp.apispec.{Schema => JsonSchema, SchemaType}
import sttp.tapir._
import sttp.tapir.json.jsoniter._

object ApiConfig {
  implicit val config: JsoniterConfig =
    JsoniterConfig.default.withSnakeCaseFieldNames.withSnakeCaseAdtLeafClassNames.withDiscriminator("type")
}
import ApiConfig._

sealed trait Payment
object Payment {
  implicit val codec: JsonValueCodec[Payment] = KindlingsJsonValueCodec.derived[Payment]
  implicit val schema: Schema[Payment] = KindlingsSchema.derived[Payment].schema
}
case class CardPayment(cardNumber: String, expiryMonth: Int) extends Payment
case class BankTransfer(accountIban: String, reference: Option[String]) extends Payment

val createPayment = endpoint.post.in("payments").in(jsonBody[Payment])

// The schemas exactly as the OpenAPI document publishes them.
val components = TapirOpenApi.toOpenAPI(createPayment, "Payments", "1.0").components.get.schemas
def component(name: String): JsonSchema = components(name).asInstanceOf[JsonSchema]

// Compares the keys of an encoded object with the (discriminator-selected) component schema.
def problems(value: Payment, schemaName: String): List[String] = {
  val obj = readFromString[Json](writeToString(value)).asObject.get
  val root = component(schemaName)
  val leaf = root.discriminator.fold(root) { d =>
    val tag = obj(d.propertyName).flatMap(_.asString).get
    component(d.mapping.get(tag).stripPrefix("#/components/schemas/"))
  }
  def allowsNull(k: String) = leaf.properties.get(k).exists {
    case s: JsonSchema => s.`type`.exists(_.contains(SchemaType.Null))
    case _             => false
  }
  val keys = obj.keys.toSet
  (keys -- leaf.properties.keySet).toList.map(k => s"field '$k' is not in the schema") ++
    leaf.required.filterNot(keys).map(k => s"required field '$k' is missing") ++
    obj.toMap.collect { case (k, Json.Null) if !allowsNull(k) => s"field '$k' is null, the schema does not allow it" }
}

println(problems(CardPayment("4111111111111111", 12), "Payment"))
// expected output:
// List()
println(problems(BankTransfer("DE89370400440532013000", Some("invoice 7")), "Payment"))
// expected output:
// List()
println(problems(BankTransfer("DE89370400440532013000", None), "Payment"))
// expected output:
// List(field 'reference' is null, the schema does not allow it)
```

In a test suite, run `problems` for a sample of every case of every body type (or generate the samples with
[ScalaCheck derivation](scalacheck-derivation.md)) and assert that the list is empty. A full JSON Schema validator
can replace the hand-written comparison when you need to check nested values and formats too.

### Golden OpenAPI document

Commit the rendered document and fail the build when it changes unexpectedly, so a changed configuration shows up in
code review (munit shown; any test framework works):

```scala
import hearth.kindlings.tapiropenapijsoniter.TapirOpenApi
import sttp.apispec.openapi.Info
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets.UTF_8

class OpenApiGoldenSpec extends munit.FunSuite {
  test("OpenAPI document matches the committed golden file") {
    val actual = TapirOpenApi.endpointsToJson(Api.endpoints, Info("Payments", "1.0"))
    val golden = Paths.get("src/test/resources/openapi.golden.json")
    if (!Files.exists(golden)) Files.write(golden, actual.getBytes(UTF_8)) // first run: record it
    assertEquals(actual, new String(Files.readAllBytes(golden), UTF_8))
  }
}
```

## Pitfalls

- **Do not mix in tapir's derivation.** `Schema.derived[A]`, `sttp.tapir.generic.auto._` and
  `sttp.tapir.generic.Configuration` ignore the Kindlings configuration. A hand-written `Schema.derived` for a nested
  type wins over Kindlings' derivation (user instances always win), so that type shows up with tapir's names. With
  `generic.auto._` in scope, `KindlingsSchema` still derives nested types itself, but any body type that has no
  Kindlings schema of its own silently gets tapir's - without the import that would be a compile error.
- **`Multiple JSON library configurations found: circe, jsoniter-scala.`** - both Kindlings JSON modules are on the
  classpath, often because `kindlings-tapir-openapi-jsoniter` brings `kindlings-jsoniter-derivation`. Add
  `PreferSchemaConfig` (see above). The hint in the error message prints a type like
  `PreferSchemaConfig[circeConfig]`, which does not exist; write
  `PreferSchemaConfig[hearth.kindlings.circederivation.Configuration]` or
  `PreferSchemaConfig[hearth.kindlings.jsoniterderivation.JsoniterConfig]`.
- **`null` on the wire for `None`.** Schemas describe `Option` fields as optional, not nullable. Use
  `withTransientNone` (jsoniter) or a `jsonPrinter` with `dropNullValues = true` (circe).
- **Enums as strings.** Use `KindlingsSchema.derived` for a sealed trait of case objects: it follows
  `withEnumAsStrings` and the constructor-name transformation of the configuration. `KindlingsSchema.derivedEnumeration`
  always uses the plain case names, so with e.g. `withSnakeCaseConstructorNames` it documents `InProgress` while the
  codec writes `in_progress`.
- **`type mismatch` with `derives KindlingsSchema`** - tapir needs `Schema[A]`; add
  `given Schema[A] = summon[KindlingsSchema[A]].schema`.
- **Platforms.** `kindlings-tapir-schema-derivation` and the JSON modules work on JVM, Scala.js and Scala Native;
  `TapirOpenApi` works on JVM and Scala.js only (it follows `tapir-openapi-docs`).
