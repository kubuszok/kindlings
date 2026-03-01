package hearth.kindlings.integrationtests

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.any.*
import io.github.iltotore.iron.constraint.numeric.*
import io.github.iltotore.iron.constraint.string.*
import hearth.MacroSuite
import hearth.kindlings.circederivation.Configuration
import hearth.kindlings.tapirschemaderivation.{KindlingsSchema, PreferSchemaConfig}
import sttp.tapir.{Schema, SchemaType}

final class IronTapirSchemaSpec extends MacroSuite {

  implicit val preferCirce: PreferSchemaConfig[Configuration] = PreferSchemaConfig[Configuration]

  group("Iron + Tapir Schema") {

    test("iron int has integer schema type") {
      val schema = KindlingsSchema.derived[Int :| Positive]
      assert(
        schema.schema.schemaType.isInstanceOf[SchemaType.SInteger[?]],
        s"Expected SInteger but got ${schema.schema.schemaType}"
      )
    }

    test("case class with iron fields derives product schema") {
      val schema = KindlingsSchema.derived[IronPerson]
      assert(
        schema.schema.schemaType.isInstanceOf[SchemaType.SProduct[?]],
        s"Expected SProduct but got ${schema.schema.schemaType}"
      )
    }
  }
}
