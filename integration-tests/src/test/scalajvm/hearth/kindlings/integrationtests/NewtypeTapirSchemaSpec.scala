package hearth.kindlings.integrationtests

import hearth.MacroSuite
import hearth.kindlings.circederivation.Configuration
import hearth.kindlings.integrationtests.newtypeExamples.*
import hearth.kindlings.tapirschemaderivation.{KindlingsSchema, PreferSchemaConfig}
import sttp.tapir.SchemaType

final class NewtypeTapirSchemaSpec extends MacroSuite {

  implicit val preferCirce: PreferSchemaConfig[Configuration] = PreferSchemaConfig[Configuration]

  group("scala-newtype + Tapir Schema") {

    test("newtype over Int has integer schema type") {
      val schema = KindlingsSchema.derived[NewtypeUserId]
      assert(
        schema.schema.schemaType.isInstanceOf[SchemaType.SInteger[?]],
        s"Expected SInteger but got ${schema.schema.schemaType}"
      )
    }

    test("case class with newtype fields derives product schema") {
      val schema = KindlingsSchema.derived[NewtypeUser]
      assert(
        schema.schema.schemaType.isInstanceOf[SchemaType.SProduct[?]],
        s"Expected SProduct but got ${schema.schema.schemaType}"
      )
    }
  }
}
