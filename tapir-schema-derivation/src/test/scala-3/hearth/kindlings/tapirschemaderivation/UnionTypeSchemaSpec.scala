package hearth.kindlings.tapirschemaderivation

import hearth.MacroSuite
import io.circe.derivation.Configuration
import sttp.tapir.SchemaType

@scala.annotation.nowarn
final class UnionTypeSchemaSpec extends MacroSuite {
  // Config preference resolved via scalac setting:
  // -Xmacro-settings:tapirSchemaDerivation.preferConfig=circe
  implicit val config: Configuration = Configuration.default

  group("Scala 3 union type schemas") {
    test("derives schema for String | Int union") {
      val schema = KindlingsSchema.derived[String | Int].schema
      schema.schemaType match {
        case c: SchemaType.SCoproduct[?] =>
          assert(c.subtypes.size == 2, s"Expected 2 subtypes, got ${c.subtypes.size}")
        case other =>
          fail(s"Expected SCoproduct, got $other")
      }
    }

    test("derives schema for multi-member union") {
      val schema = KindlingsSchema.derived[String | Int | Boolean].schema
      schema.schemaType match {
        case c: SchemaType.SCoproduct[?] =>
          assert(c.subtypes.size == 3, s"Expected 3 subtypes, got ${c.subtypes.size}")
        case other =>
          fail(s"Expected SCoproduct, got $other")
      }
    }
  }
}
