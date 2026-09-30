package hearth.kindlings.tapirschemaderivation

import hearth.MacroSuite
import hearth.kindlings.circederivation.Configuration
import sttp.tapir.{Schema, SchemaType}

// Opaque types are defined in their own object, so that outside of it they are not dealiased to String.
object OpaqueMapKeyExamples {
  opaque type UserId = String
  object UserId {
    def apply(value: String): UserId = value
    extension (id: UserId) def value: String = id
  }

  opaque type Counter = Int
  object Counter {
    def apply(value: Int): Counter = value
    extension (c: Counter) def value: Int = c
  }
}
import OpaqueMapKeyExamples.*

case class WithOpaqueMapKey(counts: Map[UserId, Int])
case class WithNonStringOpaqueMapKey(counts: Map[Counter, Int])

final class OpaqueMapKeySpec extends MacroSuite {

  implicit val config: Configuration = Configuration.default
  implicit val preferCirce: PreferSchemaConfig[Configuration] = PreferSchemaConfig[Configuration]

  group("KindlingsSchema.derived for maps with opaque type keys") {

    test("map field keyed by an opaque type over String is derived as SOpenProduct") {
      val schema = KindlingsSchema.derived[WithOpaqueMapKey].schema
      schema.schemaType match {
        case p: SchemaType.SProduct[WithOpaqueMapKey] =>
          val field = p.fields.find(_.name.name == "counts").get
          (field.schema.schemaType: SchemaType[?]) match {
            case op0: SchemaType.SOpenProduct[?, ?] =>
              val op = op0.asInstanceOf[SchemaType.SOpenProduct[Any, Any]]
              assertEquals(op.valueSchema.schemaType: Any, SchemaType.SInteger[Any](): Any)
              assertEquals(op.mapFieldValues(Map(UserId("a") -> 1)), Map[String, Any]("a" -> 1))
            case other => fail(s"Expected SOpenProduct, got: $other")
          }
        case other =>
          fail(s"Expected SProduct, got: $other")
      }
    }

    test("map field keyed by an opaque type with user-provided Schema[Map[K, V]] uses it") {
      given Schema[Map[UserId, Int]] =
        Schema.schemaForMap[UserId, Int](_.value).description("custom map schema")

      val schema = KindlingsSchema.derived[WithOpaqueMapKey].schema
      schema.schemaType match {
        case p: SchemaType.SProduct[WithOpaqueMapKey] =>
          val field = p.fields.find(_.name.name == "counts").get
          assertEquals(field.schema.description, Some("custom map schema"))
        case other =>
          fail(s"Expected SProduct, got: $other")
      }
    }

    test("map field keyed by a non-String opaque type with user-provided Schema[Map[K, V]] uses it") {
      given Schema[Map[Counter, Int]] =
        Schema.schemaForMap[Counter, Int](_.value.toString).description("custom map schema")

      val schema = KindlingsSchema.derived[WithNonStringOpaqueMapKey].schema
      schema.schemaType match {
        case p: SchemaType.SProduct[WithNonStringOpaqueMapKey] =>
          val field = p.fields.find(_.name.name == "counts").get
          assertEquals(field.schema.description, Some("custom map schema"))
        case other =>
          fail(s"Expected SProduct, got: $other")
      }
    }
  }
}
