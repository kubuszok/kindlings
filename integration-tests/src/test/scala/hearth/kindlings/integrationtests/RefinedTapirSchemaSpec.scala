package hearth.kindlings.integrationtests

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.Positive
import hearth.MacroSuite
import hearth.kindlings.circederivation.Configuration
import hearth.kindlings.tapirschemaderivation.{KindlingsSchema, PreferSchemaConfig}
import sttp.tapir.SchemaType

final class RefinedTapirSchemaSpec extends MacroSuite {

  implicit val preferCirce: PreferSchemaConfig[Configuration] = PreferSchemaConfig[Configuration]

  group("Refined + Tapir Schema") {

    test("refined string has string schema type") {
      val schema = KindlingsSchema.derived[String Refined NonEmpty]
      assert(
        schema.schema.schemaType.isInstanceOf[SchemaType.SString[?]],
        s"Expected SString but got ${schema.schema.schemaType}"
      )
    }

    test("refined int has integer schema type") {
      val schema = KindlingsSchema.derived[Int Refined Positive]
      assert(
        schema.schema.schemaType.isInstanceOf[SchemaType.SInteger[?]],
        s"Expected SInteger but got ${schema.schema.schemaType}"
      )
    }

    test("case class with refined fields derives product schema") {
      val schema = KindlingsSchema.derived[RefinedPerson]
      assert(
        schema.schema.schemaType.isInstanceOf[SchemaType.SProduct[?]],
        s"Expected SProduct but got ${schema.schema.schemaType}"
      )
    }
  }
}
