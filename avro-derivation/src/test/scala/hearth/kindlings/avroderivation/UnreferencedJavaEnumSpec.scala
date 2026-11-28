package hearth.kindlings.avroderivation

import hearth.MacroSuite

final case class AvroUnrefEnumHolder(mode: java.nio.file.AccessMode)

// `java.nio.file.AccessMode`'s constants are never referred to by the code of this test module: on Scala 3 such a Java
// enum read from a class file has no children for Hearth until they are completed
// (docs/research/hearth-bug-java-enum-unreferenced-constants.md). Do not write `AccessMode.READ` & co. in this module's
// tests; values are obtained with `valueOf`.
final class UnreferencedJavaEnumSpec extends MacroSuite {

  test("a JDK enum none of whose constants is referred to is derivable") {
    val schema = AvroSchemaFor.schemaOf[AvroUnrefEnumHolder]
    assertEquals(schema.getField("mode").schema.getEnumSymbols.toString, "[READ, WRITE, EXECUTE]")
    val holder = AvroUnrefEnumHolder(java.nio.file.AccessMode.valueOf("WRITE"))
    val bytes = AvroIO.toBinary(holder)(AvroEncoder.derived[AvroUnrefEnumHolder])
    AvroIO.fromBinary[AvroUnrefEnumHolder](bytes)(AvroDecoder.derived[AvroUnrefEnumHolder]) ==> holder
  }
}
