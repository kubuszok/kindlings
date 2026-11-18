// Deliberately in the empty package (no `package` clause): types declared here have no package namespace.

import hearth.kindlings.avroderivation.AvroSchemaFor

case class AvroEmptyPkgRecord(a: Int)
sealed trait AvroEmptyPkgEnum
case object AvroEmptyPkgA extends AvroEmptyPkgEnum
case object AvroEmptyPkgB extends AvroEmptyPkgEnum

/** A type in the empty package has no namespace on both Scala versions (as avro4s 5 on Scala 3 does; avro4s 4 on Scala
  * 2 writes "empty" for records and "<empty>" - not a valid Avro namespace - for enums).
  */
final class AvroEmptyPackageSpec extends hearth.MacroSuite {

  group("types in the empty package") {

    test("a record has no namespace") {
      val schema = AvroSchemaFor.schemaOf[AvroEmptyPkgRecord]
      assertEquals(schema.getNamespace, null)
      assertEquals(schema.getFullName, "AvroEmptyPkgRecord")
    }

    test("an enum has no namespace") {
      val schema = AvroSchemaFor.schemaOf[AvroEmptyPkgEnum]
      assertEquals(schema.getNamespace, null)
      assertEquals(schema.getFullName, "AvroEmptyPkgEnum")
    }
  }
}
