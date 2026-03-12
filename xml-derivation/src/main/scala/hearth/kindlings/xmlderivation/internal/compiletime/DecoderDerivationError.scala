package hearth.kindlings.xmlderivation.internal.compiletime

sealed abstract private[compiletime] class DecoderDerivationError(val message: String) extends Exception(message)
private[compiletime] object DecoderDerivationError {
  final case class UnsupportedType(typeName: String, reasons: List[String])
      extends DecoderDerivationError(
        s"Cannot derive XmlDecoder for type $typeName:\n${reasons.mkString("\n")}"
      )
}
