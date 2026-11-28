package hearth.kindlings.xmlderivation

import hearth.MacroSuite

final case class XmlPrivate(a: Int, private val b: String, c: Boolean) {
  def bValue: String = b
}

// Hearth's `caseFieldValuesAt` has no `private val` constructor parameters (they have no accessor the generated code could
// call): derivations which read fields include them anyway (through `Product.productElement`), matched by name, so the
// fields after them are not shifted.
final class PrivateFieldsSpec extends MacroSuite {

  test("private val constructor fields are encoded and decoded") {
    val value = XmlPrivate(1, "two", c = true)
    val xml = KindlingsXmlEncoder.toXmlString(value, "p")
    assert(xml.contains("two"), xml)
    KindlingsXmlDecoder.fromXmlString[XmlPrivate](xml) ==> Right(value)
  }
}
