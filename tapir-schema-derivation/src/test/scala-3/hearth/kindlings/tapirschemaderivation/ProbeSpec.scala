package hearth.kindlings.tapirschemaderivation

import hearth.MacroSuite
import io.circe.derivation.Configuration
import sttp.tapir.SchemaType

@scala.annotation.nowarn
final class ProbeSpec extends MacroSuite {
  // Config preference resolved via scalac setting:
  // -Xmacro-settings:tapirSchemaDerivation.preferConfig=circe
  implicit val config: Configuration = Configuration.default

  test("derive schema for simple case class from scala-3/ source root") {
    val schema = KindlingsSchema.derived[SimplePerson].schema
    schema.schemaType match {
      case p: SchemaType.SProduct[SimplePerson] =>
        val fieldNames = p.fields.map(_.name.name)
        assertEquals(fieldNames, List("name", "age"))
      case other =>
        fail(s"Expected SProduct, got: $other")
    }
  }
}
