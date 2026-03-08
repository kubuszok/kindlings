package hearth.kindlings.xmlderivation

trait KindlingsXmlDecoder[A] extends XmlDecoder[A] {
  def decode(elem: scala.xml.Elem): Either[XmlDecodingError, A]
}
object KindlingsXmlDecoder extends KindlingsXmlDecoderCompanionCompat {

  /** Special type - if its implicit is in scope then macros will log the derivation process.
    *
    * @see
    *   [[hearth.kindlings.xmlderivation.debug.logDerivationForKindlingsXmlDecoder]] for details
    */
  sealed trait LogDerivation
  object LogDerivation extends LogDerivation
}
