package hearth.kindlings.tapirschemaderivation

import hearth.MacroSuite
import hearth.kindlings.jsoniterderivation.JsoniterConfig
import sttp.tapir.{Schema, SchemaType, Validator}

final case class TapirUnrefEnumHolder(mode: java.nio.file.AccessMode)

object TapirUnrefEnumSchemas {
  implicit val config: JsoniterConfig = JsoniterConfig.default
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val schema: Schema[TapirUnrefEnumHolder] = KindlingsSchema.derived[TapirUnrefEnumHolder].schema
}

// `java.nio.file.AccessMode`'s constants are never referred to by the code of this test module: on Scala 3 such a Java
// enum read from a class file has no children for Hearth until they are completed
// (docs/research/hearth-bug-java-enum-unreferenced-constants.md). Do not write `AccessMode.READ` & co. in this module's
// tests; values are obtained with `valueOf`.
final class UnreferencedJavaEnumJvmSpec extends MacroSuite {

  test("a JDK enum none of whose constants is referred to is an enumeration of its constants in declaration order") {
    val mode: Schema[?] = TapirUnrefEnumSchemas.schema.schemaType match {
      case p: SchemaType.SProduct[?] => p.fields.head.schema: Schema[?]
      case other                     => fail(s"Expected SProduct, got: $other")
    }
    mode.validator match {
      case e: Validator.Enumeration[?] =>
        val en = e.asInstanceOf[Validator.Enumeration[Any]]
        assertEquals(en.possibleValues.flatMap(v => en.encode.flatMap(f => f(v))), List("READ", "WRITE", "EXECUTE"))
      case other => fail(s"Expected an enumeration validator, got: $other")
    }
  }
}
