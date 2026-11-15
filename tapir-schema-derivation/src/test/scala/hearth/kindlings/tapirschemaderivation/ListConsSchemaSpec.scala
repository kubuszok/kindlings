package hearth.kindlings.tapirschemaderivation

import hearth.MacroSuite
import sttp.tapir.SchemaType

final case class TsConsBox(xs: ::[Int])

/** `::` (a non-empty `List`): an array schema that is required (it cannot be empty, so there is no `[]` to fill in). */
final class ListConsSchemaSpec extends MacroSuite {

  test("a field of type :: is a required array") {
    KindlingsSchema.derived[TsConsBox].schema.schemaType match {
      case p: SchemaType.SProduct[TsConsBox] =>
        val field = p.fields.find(_.name.name == "xs").getOrElse(fail("no xs field"))
        assertEquals(field.schema.isOptional, false)
        assert(field.schema.schemaType.isInstanceOf[SchemaType.SArray[?, ?]], field.schema.schemaType.toString)
      case other => fail(s"Expected SProduct, got: $other")
    }
  }
}
