package hearth.kindlings.xmlderivation.internal.compiletime

sealed abstract private[compiletime] class DecoderDerivationError(val message: String) extends Exception(message)
private[compiletime] object DecoderDerivationError {
  final case class UnsupportedType(typeName: String, reasons: List[String])
      extends DecoderDerivationError(
        s"Cannot derive XmlDecoder for type $typeName:\n${reasons.mkString("\n")}"
      )
  final case class CannotConstructType(typeName: String, constructorError: Option[String] = None)
      extends DecoderDerivationError(
        constructorError.fold(s"Cannot construct $typeName")(err => s"Cannot construct $typeName: $err")
      )
  final case class SingletonDisappeared(typeName: String)
      extends DecoderDerivationError(
        s"Singleton disappeared for $typeName"
      )
}
