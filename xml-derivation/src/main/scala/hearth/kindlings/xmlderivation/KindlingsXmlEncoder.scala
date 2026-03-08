package hearth.kindlings.xmlderivation

trait KindlingsXmlEncoder[A] extends XmlEncoder[A] {
  def encode(value: A, elementName: String): scala.xml.Elem
}
object KindlingsXmlEncoder extends KindlingsXmlEncoderCompanionCompat {

  /** Special type - if its implicit is in scope then macros will log the derivation process.
    *
    * @see
    *   [[hearth.kindlings.xmlderivation.debug.logDerivationForKindlingsXmlEncoder]] for details
    */
  sealed trait LogDerivation
  object LogDerivation extends LogDerivation
}
