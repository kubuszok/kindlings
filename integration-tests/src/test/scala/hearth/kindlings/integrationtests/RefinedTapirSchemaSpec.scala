package hearth.kindlings.integrationtests

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.Positive
import hearth.MacroSuite
import hearth.kindlings.circederivation.Configuration
import hearth.kindlings.tapirschemaderivation.{KindlingsSchema, PreferSchemaConfig}
import eu.timepit.refined.refineV
import sttp.tapir.{Schema, SchemaType}

final class RefinedTapirSchemaSpec extends MacroSuite {

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

    test("map keyed by refined String derives a String-keyed map schema") {
      val key = refineV[NonEmpty]("a").toOption.get
      assertStringKeyedMapSchema(
        KindlingsSchema.derived[WithRefinedKeyMap].schema,
        Map(key -> 1),
        Map[String, Any]("a" -> 1)
      )
    }

    test("map keyed by refined String uses user-provided Schema[Map[K, V]]") {
      implicit val custom: Schema[Map[String Refined NonEmpty, Int]] =
        Schema.schemaForMap[String Refined NonEmpty, Int](_.value).description("custom map schema")
      assertCustomMapSchema(KindlingsSchema.derived[WithRefinedKeyMap].schema)
    }

    test("map keyed by refined Int uses user-provided Schema[Map[K, V]]") {
      implicit val custom: Schema[Map[Int Refined Positive, Int]] =
        Schema.schemaForMap[Int Refined Positive, Int](_.value.toString).description("custom map schema")
      assertCustomMapSchema(KindlingsSchema.derived[WithRefinedIntKeyMap].schema)
    }

    test("map keyed by refined Int without Schema[Map[K, V]] fails to compile") {
      compileErrors(
        "hearth.kindlings.tapirschemaderivation.KindlingsSchema.derived[hearth.kindlings.integrationtests.WithRefinedIntKeyMap]"
      ).check("Cannot derive tapir Schema for Map with key type")
    }
  }
}
