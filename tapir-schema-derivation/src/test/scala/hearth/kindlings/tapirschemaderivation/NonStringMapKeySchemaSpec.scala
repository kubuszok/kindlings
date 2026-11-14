package hearth.kindlings.tapirschemaderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{writeToString, JsonValueCodec}
import hearth.MacroSuite
import hearth.kindlings.circederivation.{Configuration, KindlingsEncoder}
import hearth.kindlings.jsoniterderivation.{JsoniterConfig, KindlingsJsonValueCodec}
import io.circe.Encoder
import sttp.tapir.{Schema, SchemaType}

import java.util.UUID

final case class KeyInt(value: Int) extends AnyVal
final case class KeyUuid(value: UUID) extends AnyVal

final case class NonStringKeys(
    ints: Map[Int, String],
    longs: Map[Long, Int],
    uuids: Map[UUID, Int],
    wrapped: Map[KeyInt, String],
    nested: Map[KeyUuid, String]
)
object NonStringKeys {
  val sample: NonStringKeys = NonStringKeys(
    ints = Map(7 -> "x", -1 -> "y"),
    longs = Map(10000000000L -> 1),
    uuids = Map(UUID.fromString("00000000-0000-0000-0000-000000000001") -> 1),
    wrapped = Map(KeyInt(3) -> "z"),
    nested = Map(KeyUuid(UUID.fromString("00000000-0000-0000-0000-000000000002")) -> "w")
  )
}

object NonStringKeysJsoniter {
  implicit val config: JsoniterConfig = JsoniterConfig.default
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val schema: Schema[NonStringKeys] = KindlingsSchema.derived[NonStringKeys].schema
  val codec: JsonValueCodec[NonStringKeys] = KindlingsJsonValueCodec.derived[NonStringKeys]
}

object NonStringKeysCirce {
  implicit val config: Configuration = Configuration.default
  val schema: Schema[NonStringKeys] = KindlingsSchema.derived[NonStringKeys].schema
  val encoder: Encoder[NonStringKeys] = KindlingsEncoder.derived[NonStringKeys]
}

/** Maps keyed by any type the JSON library writes as an object key (primitives, `UUID`, value types over them, ...) are
  * `SOpenProduct`s, whose keys are the strings the codecs write.
  */
final class NonStringMapKeySchemaSpec extends MacroSuite {

  private def openProducts(schema: Schema[?]): Map[String, SchemaType.SOpenProduct[Any, Any]] =
    schema.schemaType match {
      case p: SchemaType.SProduct[?] =>
        p.fields.map { f =>
          (f.schema.schemaType: SchemaType[?]) match {
            case op: SchemaType.SOpenProduct[?, ?] => f.name.encodedName -> op.asInstanceOf[SchemaType.SOpenProduct[Any, Any]]
            case other                             => fail(s"${f.name.encodedName}: expected SOpenProduct, got: $other")
          }
        }.toMap
      case other => fail(s"Expected SProduct, got: $other")
    }

  private val fieldValues: Map[String, Map[?, ?]] = {
    val s = NonStringKeys.sample
    Map(
      "ints" -> s.ints,
      "longs" -> s.longs,
      "uuids" -> s.uuids,
      "wrapped" -> s.wrapped,
      "nested" -> s.nested
    )
  }

  /** The keys of every map field of the JSON object written by a codec. */
  private def writtenKeys(json: io.circe.Json): Map[String, Set[String]] =
    json.asObject.get.toMap.map { case (field, value) => field -> value.asObject.get.keys.toSet }

  private def check(schema: Schema[NonStringKeys], json: io.circe.Json): Unit = {
    val products = openProducts(schema)
    val written = writtenKeys(json)
    fieldValues.foreach { case (field, map) =>
      assertEquals(products(field).mapFieldValues(map).keySet, written(field), s"keys of $field")
    }
    val errors = SchemaConformance.errors(schema, json)
    assert(errors.isEmpty, s"${json.noSpaces} does not conform to the schema:\n${errors.mkString("\n")}")
  }

  test("jsoniter: the schema's keys are the keys the codec writes") {
    val json = io.circe.parser.parse(writeToString(NonStringKeys.sample)(NonStringKeysJsoniter.codec)).toOption.get
    check(NonStringKeysJsoniter.schema, json)
  }

  test("circe: the schema's keys are the keys the encoder writes") {
    check(NonStringKeysCirce.schema, NonStringKeysCirce.encoder(NonStringKeys.sample))
  }
}
