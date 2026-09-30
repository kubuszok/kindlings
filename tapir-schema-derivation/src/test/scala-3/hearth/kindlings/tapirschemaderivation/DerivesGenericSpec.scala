package hearth.kindlings.tapirschemaderivation

import hearth.MacroSuite
import hearth.kindlings.circederivation.Configuration
import sttp.tapir.{Schema, SchemaType}

// `derives` on a generic type makes Scala 3 synthesize
// `given [A](using KindlingsSchema[A]): KindlingsSchema[DerivedBox[A]]`
case class DerivedBox[A](value: A) derives KindlingsSchema

final class DerivesGenericSpec extends MacroSuite {

  implicit val config: Configuration = Configuration.default
  implicit val preferCirce: PreferSchemaConfig[Configuration] = PreferSchemaConfig[Configuration]

  private def check[A](schema: Schema[DerivedBox[A]], expected: SchemaType[?], typeParam: String): Unit = {
    assertEquals(schema.name.map(_.typeParameterShortNames), Some(List(typeParam)))
    schema.schemaType match {
      case p: SchemaType.SProduct[DerivedBox[A] @unchecked] =>
        assertEquals(p.fields.map(_.name.name), List("value"))
        assertEquals(p.fields.head.schema.schemaType.getClass: Any, expected.getClass: Any)
      case other => fail(s"Expected SProduct, got: $other")
    }
  }

  group("derives KindlingsSchema on a generic type") {

    test("with a built-in type parameter") {
      check(summon[KindlingsSchema[DerivedBox[Int]]].schema, SchemaType.SInteger(), "Int")
      check(summon[KindlingsSchema[DerivedBox[String]]].schema, SchemaType.SString(), "String")
    }

    test("with a case class type parameter") {
      check(summon[KindlingsSchema[DerivedBox[SimplePerson]]].schema, SchemaType.SProduct(Nil), "SimplePerson")
    }

    test("nested in a derived case class") {
      val schema = KindlingsSchema.derived[WithDerivedBox].schema
      schema.schemaType match {
        case p: SchemaType.SProduct[WithDerivedBox] =>
          assertEquals(p.fields.map(_.name.name), List("box"))
        case other => fail(s"Expected SProduct, got: $other")
      }
    }
  }
}

case class WithDerivedBox(box: DerivedBox[Long])
