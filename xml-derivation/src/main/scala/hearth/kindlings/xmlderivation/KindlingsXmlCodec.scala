package hearth.kindlings.xmlderivation

trait KindlingsXmlCodec[A] extends XmlEncoder[A] with XmlDecoder[A]

object KindlingsXmlCodec extends KindlingsXmlCodecCompanionCompat {

  /** Special type - if its implicit is in scope then macros will log the derivation process.
    *
    * @see
    *   [[hearth.kindlings.xmlderivation.debug.logDerivationForKindlingsXmlEncoder]] and
    *   [[hearth.kindlings.xmlderivation.debug.logDerivationForKindlingsXmlDecoder]] for details
    */
  sealed trait LogDerivation
  object LogDerivation extends LogDerivation
}
