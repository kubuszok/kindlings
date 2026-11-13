package hearth.kindlings.tapirschemaderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{writeToString, JsonValueCodec}
import hearth.MacroSuite
import hearth.kindlings.jsoniterderivation.{JsoniterConfig, KindlingsJsonValueCodec}
import sttp.tapir.{Schema, SchemaType, Validator}

final case class FidJavaEnumHolder(color: FidJavaColor)

object FidJavaEnumDiscriminator {
  // As in jsoniter-scala, Java enums are strings whatever the discriminator, named by javaEnumValueNameMapper
  implicit val config: JsoniterConfig =
    JsoniterConfig.default.withDiscriminator("type").withJavaEnumValueNameMapper((s: String) => s.toLowerCase)
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val schema: Schema[FidJavaEnumHolder] = KindlingsSchema.derived[FidJavaEnumHolder].schema
  val codec: JsonValueCodec[FidJavaEnumHolder] = KindlingsJsonValueCodec.derived[FidJavaEnumHolder]
}

/** Java enums with a JsoniterConfig (JVM-only, because the Java enum fixture is a Java source). */
final class JsoniterJavaEnumSchemaJvmSpec extends MacroSuite {

  test("Java enums are strings named by javaEnumValueNameMapper, whatever the discriminator") {
    val color: Schema[?] = FidJavaEnumDiscriminator.schema.schemaType match {
      case p: SchemaType.SProduct[?] => p.fields.head.schema: Schema[?]
      case other                     => fail(s"Expected SProduct, got: $other")
    }
    assert(color.schemaType.isInstanceOf[SchemaType.SString[?]], color.schemaType.toString)
    color.validator match {
      case e: Validator.Enumeration[?] =>
        val en = e.asInstanceOf[Validator.Enumeration[Any]]
        assertEquals(en.possibleValues.flatMap(v => en.encode.flatMap(f => f(v))), List("red", "green", "blue"))
      case other => fail(s"Expected an enumeration validator, got: $other")
    }
    FidJavaColor.values().toList.foreach { c =>
      val json = writeToString(FidJavaEnumHolder(c))(FidJavaEnumDiscriminator.codec)
      val errors = SchemaConformance.errors(FidJavaEnumDiscriminator.schema, io.circe.parser.parse(json).toOption.get)
      assert(errors.isEmpty, s"$json: ${errors.mkString("; ")}")
    }
  }
}
