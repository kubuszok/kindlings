package hearth.kindlings.integrationtests

import hearth.MacroSuite
import hearth.kindlings.circederivation.Configuration
import hearth.kindlings.integrationtests.newtypeExamples.*
import hearth.kindlings.tapirschemaderivation.{KindlingsSchema, PreferSchemaConfig}
import sttp.tapir.{Schema, SchemaType}

final class NewtypeTapirSchemaSpec extends MacroSuite {

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

    test("map keyed by newtype over String derives a String-keyed map schema") {
      assertStringKeyedMapSchema(
        KindlingsSchema.derived[WithNewtypeKeyMap].schema,
        Map(NewtypeUsername("a") -> 1),
        Map[String, Any]("a" -> 1)
      )
    }

    test("map keyed by newtype over String uses user-provided Schema[Map[K, V]]") {
      implicit val custom: Schema[Map[NewtypeUsername, Int]] =
        Schema.schemaForMap[NewtypeUsername, Int](_.value).description("custom map schema")
      assertCustomMapSchema(KindlingsSchema.derived[WithNewtypeKeyMap].schema)
    }

    test("map keyed by newtype over Int uses user-provided Schema[Map[K, V]]") {
      implicit val custom: Schema[Map[NewtypeUserId, Int]] =
        Schema.schemaForMap[NewtypeUserId, Int](_.value.toString).description("custom map schema")
      assertCustomMapSchema(KindlingsSchema.derived[WithNewtypeIntKeyMap].schema)
    }
  }
}
