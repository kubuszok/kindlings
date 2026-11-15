package hearth.kindlings.tapirschemaderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{writeToString, JsonValueCodec}
import hearth.MacroSuite
import hearth.kindlings.circederivation.{Configuration, KindlingsEncoder}
import hearth.kindlings.jsoniterderivation.{JsoniterConfig, KindlingsJsonValueCodec}
import sttp.tapir.{Schema, SchemaType}

enum KeyFruit {
  case Apple, BlueBerry
}

final case class WithEnumMapKey(counts: Map[KeyFruit, Int])
object WithEnumMapKey {
  val sample: WithEnumMapKey = WithEnumMapKey(Map(KeyFruit.Apple -> 1, KeyFruit.BlueBerry -> 2))
}

object EnumMapKeyJsoniter {
  given JsoniterConfig = JsoniterConfig.default
  given PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val schema: Schema[WithEnumMapKey] = KindlingsSchema.derived[WithEnumMapKey].schema
  val codec: JsonValueCodec[WithEnumMapKey] = KindlingsJsonValueCodec.derived[WithEnumMapKey]
}

object EnumMapKeyCirce {
  given Configuration = Configuration.default.withSnakeCaseConstructorNames
  val schema: Schema[WithEnumMapKey] = KindlingsSchema.derived[WithEnumMapKey].schema
  val encoder: io.circe.Encoder[WithEnumMapKey] = KindlingsEncoder.derived[WithEnumMapKey]
}

/** Maps keyed by a Scala 3 `enum` are `SOpenProduct`s keyed by the names the codecs write for the cases. */
final class EnumMapKeySpec extends MacroSuite {

  private def check(schema: Schema[WithEnumMapKey], json: io.circe.Json): Unit = {
    val keys = schema.schemaType match {
      case p: SchemaType.SProduct[?] =>
        (p.fields.head.schema.schemaType: SchemaType[?]) match {
          case op: SchemaType.SOpenProduct[?, ?] =>
            op.asInstanceOf[SchemaType.SOpenProduct[Any, Any]].mapFieldValues(WithEnumMapKey.sample.counts).keySet
          case other => fail(s"expected SOpenProduct, got: $other")
        }
      case other => fail(s"expected SProduct, got: $other")
    }
    assertEquals(keys, json.hcursor.downField("counts").keys.get.toSet)
    val errors = SchemaConformance.errors(schema, json)
    assert(errors.isEmpty, s"${json.noSpaces} does not conform to the schema:\n${errors.mkString("\n")}")
  }

  test("jsoniter: the schema's keys are the case names the codec writes") {
    val json = io.circe.parser.parse(writeToString(WithEnumMapKey.sample)(EnumMapKeyJsoniter.codec)).toOption.get
    check(EnumMapKeyJsoniter.schema, json)
  }

  test("circe: the schema's keys are the case names the encoder writes") {
    check(EnumMapKeyCirce.schema, EnumMapKeyCirce.encoder(WithEnumMapKey.sample))
  }
}
