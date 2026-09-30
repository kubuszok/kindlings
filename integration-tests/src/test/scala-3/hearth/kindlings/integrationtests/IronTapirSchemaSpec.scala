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

  /** Asserts that the `values` field is a Map schema (SOpenProduct of Ints), whose keys are converted to Strings. */
  private def assertStringKeyedMapSchema[A](schema: Schema[A], mapValue: Any, expected: Map[String, Any]): Unit =
    schema.schemaType match {
      case p: SchemaType.SProduct[A @unchecked] =>
        val field = p.fields.find(_.name.name == "values").get
        (field.schema.schemaType: SchemaType[?]) match {
          case op0: SchemaType.SOpenProduct[?, ?] =>
            val op = op0.asInstanceOf[SchemaType.SOpenProduct[Any, Any]]
            assertEquals(op.valueSchema.schemaType: Any, SchemaType.SInteger[Any](): Any)
            assertEquals(op.mapFieldValues(mapValue), expected)
          case other => fail(s"Expected SOpenProduct, got: $other")
        }
      case other => fail(s"Expected SProduct, got: $other")
    }

  /** Asserts that the `values` field uses the user-provided Schema (recognized by its description). */
  private def assertCustomMapSchema[A](schema: Schema[A]): Unit =
    schema.schemaType match {
      case p: SchemaType.SProduct[A @unchecked] =>
        val field = p.fields.find(_.name.name == "values").get
        assertEquals(field.schema.description, Some("custom map schema"))
      case other => fail(s"Expected SProduct, got: $other")
    }

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

    test("map keyed by iron String derives a String-keyed map schema") {
      assertStringKeyedMapSchema(
        KindlingsSchema.derived[WithIronKeyMap].schema,
        Map("a".refineUnsafe[Not[Blank]] -> 1),
        Map[String, Any]("a" -> 1)
      )
    }

    test("map keyed by iron String uses user-provided Schema[Map[K, V]]") {
      implicit val custom: Schema[Map[String :| Not[Blank], Int]] =
        Schema.schemaForMap[String :| Not[Blank], Int](identity).description("custom map schema")
      assertCustomMapSchema(KindlingsSchema.derived[WithIronKeyMap].schema)
    }

    test("map keyed by iron Int uses user-provided Schema[Map[K, V]]") {
      implicit val custom: Schema[Map[Int :| Positive, Int]] =
        Schema.schemaForMap[Int :| Positive, Int](_.toString).description("custom map schema")
      assertCustomMapSchema(KindlingsSchema.derived[WithIronIntKeyMap].schema)
    }
  }
}
