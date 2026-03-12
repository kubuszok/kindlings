package hearth.kindlings.xmlderivation.internal.compiletime

sealed abstract private[compiletime] class EncoderDerivationError(val message: String) extends Exception(message)
private[compiletime] object EncoderDerivationError {
  final case class UnsupportedType(typeName: String, reasons: List[String])
      extends EncoderDerivationError(
        s"Cannot derive XmlEncoder for type $typeName:\n${reasons.mkString("\n")}"
      )
}
