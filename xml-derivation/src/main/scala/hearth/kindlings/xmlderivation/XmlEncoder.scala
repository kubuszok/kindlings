package hearth.kindlings.xmlderivation

trait XmlEncoder[A] {
  def encode(value: A, elementName: String): scala.xml.Elem
}
