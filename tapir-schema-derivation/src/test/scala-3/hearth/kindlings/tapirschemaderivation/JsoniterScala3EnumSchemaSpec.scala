package hearth.kindlings.tapirschemaderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{writeToString, JsonValueCodec}
import hearth.MacroSuite
import hearth.kindlings.jsoniterderivation.{JsoniterConfig, KindlingsJsonValueCodec}
import hearth.kindlings.jsoniterderivation.annotations.named
import sttp.tapir.{Schema, SchemaType}

enum FidPlanet {
  case Mercury
  case Venus
  @named("blue-one") case Earth
}

enum FidShape {
  case Dot
  case Square(side: Int)
}

final case class FidScala3Holder(planet: FidPlanet, shape: FidShape)

object FidScala3FullNames {
  // the leaves are named by their fully-qualified names (`pkg.Enum.Case`), as in jsoniter-scala
  implicit val config: JsoniterConfig =
    JsoniterConfig.default.withDiscriminator("type").withAdtLeafClassNameMapper((s: String) => s)
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val schema: Schema[FidScala3Holder] = KindlingsSchema.derived[FidScala3Holder].schema
  val codec: JsonValueCodec[FidScala3Holder] = KindlingsJsonValueCodec.derived[FidScala3Holder]
}

object FidScala3Default {
  implicit val config: JsoniterConfig = JsoniterConfig.default
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val schema: Schema[FidScala3Holder] = KindlingsSchema.derived[FidScala3Holder].schema
  val codec: JsonValueCodec[FidScala3Holder] = KindlingsJsonValueCodec.derived[FidScala3Holder]
}

/** Scala 3 enums with a JsoniterConfig: the schemas describe what the jsoniter codecs write. */
final class JsoniterScala3EnumSchemaSpec extends MacroSuite {

  private val samples = List(
    FidScala3Holder(FidPlanet.Mercury, FidShape.Dot),
    FidScala3Holder(FidPlanet.Earth, FidShape.Square(2))
  )

  List(
    ("full names", FidScala3FullNames.schema, FidScala3FullNames.codec),
    ("default", FidScala3Default.schema, FidScala3Default.codec)
  ).foreach { case (name, schema, codec) =>
    test(s"jsoniter codec output conforms to the derived schema ($name)") {
      samples.foreach { sample =>
        val json = writeToString(sample)(codec)
        val errors = SchemaConformance.errors(schema, io.circe.parser.parse(json).toOption.get)
        assert(errors.isEmpty, s"$json does not conform to the schema:\n${errors.mkString("\n")}")
      }
    }
  }

  test("enum cases are passed to adtLeafClassNameMapper with their fully-qualified names") {
    val shape = FidScala3FullNames.schema.schemaType match {
      case p: SchemaType.SProduct[?] => p.fields.find(_.name.name == "shape").get.schema
      case other                     => fail(s"Expected SProduct, got: $other")
    }
    shape.schemaType match {
      case c: SchemaType.SCoproduct[?] =>
        assertEquals(
          c.discriminator.map(_.mapping.keySet).getOrElse(Set.empty),
          Set("hearth.kindlings.tapirschemaderivation.FidShape.Dot", "hearth.kindlings.tapirschemaderivation.FidShape.Square")
        )
      case other => fail(s"Expected SCoproduct, got: $other")
    }
  }
}
