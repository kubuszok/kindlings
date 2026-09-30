package hearth.kindlings.integrationtests

import hearth.MacroSuite
import hearth.kindlings.circederivation.Configuration
import hearth.kindlings.tapirschemaderivation.{KindlingsSchema, PreferSchemaConfig}
import sttp.tapir.{Schema, SchemaType}

final class NeotypeTapirSchemaSpec extends MacroSuite {

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

  group("Neotype + Tapir Schema") {

    test("neotype over Int has integer schema type") {
      val schema = KindlingsSchema.derived[NeotypeAge]
      assert(
        schema.schema.schemaType.isInstanceOf[SchemaType.SInteger[?]],
        s"Expected SInteger but got ${schema.schema.schemaType}"
      )
    }

    test("case class with neotype fields derives product schema") {
      val schema = KindlingsSchema.derived[NeotypePerson]
      assert(
        schema.schema.schemaType.isInstanceOf[SchemaType.SProduct[?]],
        s"Expected SProduct but got ${schema.schema.schemaType}"
      )
    }

    test("map keyed by neotype over String derives a String-keyed map schema") {
      assertStringKeyedMapSchema(
        KindlingsSchema.derived[WithNeotypeKeyMap].schema,
        Map(NeotypeEmail("a@b") -> 1),
        Map[String, Any]("a@b" -> 1)
      )
    }

    test("map keyed by neotype over String uses user-provided Schema[Map[K, V]]") {
      implicit val custom: Schema[Map[NeotypeEmail, Int]] =
        Schema.schemaForMap[NeotypeEmail, Int](NeotypeEmail.unwrap(_)).description("custom map schema")
      assertCustomMapSchema(KindlingsSchema.derived[WithNeotypeKeyMap].schema)
    }

    test("map keyed by neotype over Int uses user-provided Schema[Map[K, V]]") {
      implicit val custom: Schema[Map[NeotypeAge, Int]] =
        Schema.schemaForMap[NeotypeAge, Int](_.toString).description("custom map schema")
      assertCustomMapSchema(KindlingsSchema.derived[WithNeotypeIntKeyMap].schema)
    }
  }
}
