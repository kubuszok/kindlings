package hearth.kindlings.tapiropenapijsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import hearth.MacroSuite
import hearth.kindlings.jsoniterderivation.JsoniterConfig
import hearth.kindlings.tapirschemaderivation.{KindlingsSchema, PreferSchemaConfig}
import sttp.tapir.Schema
import sttp.tapir.docs.apispec.schema.TapirSchemaToJsonSchema

sealed trait E2EShape
object E2EShape {
  case object Origin extends E2EShape
  final case class Circle(radius: Int) extends E2EShape
}
final case class E2EHolder(shape: E2EShape, pair: (Int, String))

object E2EDerivation {
  implicit val config: JsoniterConfig = JsoniterConfig.default
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val schema: Schema[E2EHolder] = KindlingsSchema.derived[E2EHolder].schema
}

/** End-to-end: a tapir Schema derived by KindlingsSchema with a JsoniterConfig, rendered as JSON Schema by tapir's docs
  * interpreter and serialized with this module's codecs, describes what the jsoniter codecs write: tuples as arrays
  * (`prefixItems`), wrapped ADT leaves and case objects as strings. JVM-only, as the other tapir-docs tests.
  */
final class KindlingsSchemaJsonSchemaJvmSpec extends MacroSuite {

  test("tuples are arrays with prefixItems, ADT leaves are wrapped, case objects are string enums") {
    val jsonSchema = TapirSchemaToJsonSchema(E2EDerivation.schema, markOptionsAsNullable = false)
    val json = writeToString(jsonSchema)(OpenApiJsoniter.openapi_3_1.schemaCodec)
    assert(
      json.contains(""""pair":{"type":"array","prefixItems":[{"type":"integer","format":"int32"},{"type":"string"}]"""),
      json
    )
    assert(json.contains(""""required":["Circle"]"""), json)
    assert(json.contains(""""enum":["Origin"]"""), json)
  }
}
