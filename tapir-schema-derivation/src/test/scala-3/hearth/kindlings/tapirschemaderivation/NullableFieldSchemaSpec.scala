package hearth.kindlings.tapirschemaderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonValueCodec}
import hearth.MacroSuite
import hearth.kindlings.jsoniterderivation.{JsoniterConfig, KindlingsJsonValueCodec}
import sttp.tapir.{Schema, SchemaType}

import scala.util.Try

final case class AbsNullable(text: String | Null, n: Int)

object AbsNullableJsoniter {
  given config: JsoniterConfig = JsoniterConfig.default.withRequireCollectionFields.withRequireDefaultFields
  given prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val schema: Schema[AbsNullable] = KindlingsSchema.derived[AbsNullable].schema
  val codec: JsonValueCodec[AbsNullable] = KindlingsJsonValueCodec.derived[AbsNullable]
}

/** A Scala 3 nullable field (`T | Null`) is never required by the jsoniter decoder (absent is `null`), so the schema
  * marks it optional, with the schema of `T`.
  */
final class NullableFieldSchemaSpec extends MacroSuite {

  private def field(schema: Schema[?], name: String): Schema[?] = schema.schemaType match {
    case p: SchemaType.SProduct[?] =>
      p.fields.find(_.name.encodedName == name).map(f => f.schema: Schema[?]).getOrElse(fail(s"No field $name"))
    case other => fail(s"Expected SProduct, got: $other")
  }

  test("the decoder accepts an absent nullable field (also with requireCollectionFields/requireDefaultFields)") {
    assertEquals(
      Try(readFromString[AbsNullable]("""{"n":1}""")(AbsNullableJsoniter.codec)).toOption.map(_.text),
      Some(null)
    )
    assert(Try(readFromString[AbsNullable]("""{"text":"a"}""")(AbsNullableJsoniter.codec)).isFailure)
  }

  test("the nullable field is optional in the schema, the other one is required") {
    val text = field(AbsNullableJsoniter.schema, "text")
    assert(text.isOptional, s"text should be optional: $text")
    assertEquals(field(AbsNullableJsoniter.schema, "n").isOptional, false)
  }

  test("the nullable field has the schema of its non-null type") {
    val text = field(AbsNullableJsoniter.schema, "text")
    val element = text.schemaType match {
      case o: SchemaType.SOption[?, ?] => o.element.schemaType
      case other                       => other
    }
    assert(element.isInstanceOf[SchemaType.SString[?]], s"Expected a string schema, got: $element")
  }

  test("what the codec writes, null included, conforms to the schema") {
    List(AbsNullable(null, 1), AbsNullable("a", 2)).foreach { value =>
      val json = writeToString(value)(AbsNullableJsoniter.codec)
      val parsed = io.circe.parser.parse(json).fold(e => fail(s"Invalid JSON $json: $e"), identity)
      val errors = SchemaConformance.errors(AbsNullableJsoniter.schema, parsed)
      assert(errors.isEmpty, s"$json does not conform to the schema:\n${errors.mkString("\n")}")
    }
  }
}
