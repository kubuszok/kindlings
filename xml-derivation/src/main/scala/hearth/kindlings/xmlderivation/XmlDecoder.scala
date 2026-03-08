package hearth.kindlings.xmlderivation

trait XmlDecoder[A] {
  def decode(elem: scala.xml.Elem): Either[XmlDecodingError, A]
}
