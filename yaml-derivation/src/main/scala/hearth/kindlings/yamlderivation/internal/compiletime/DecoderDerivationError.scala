package hearth.kindlings.yamlderivation.internal.compiletime

sealed private[compiletime] trait DecoderDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object DecoderDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends DecoderDerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any decoder derivation rule:\n${reasons.mkString("\n")}"
  }
  final case class NoKeyDecoder(tpeName: String, reasons: List[String]) extends DecoderDerivationError {
    override def message: String =
      s"No YamlKeyDecoder[$tpeName] for the map's keys: none in scope and none built in or derivable ($tpeName is not " +
        "a String, a primitive, BigInt, BigDecimal, UUID, an enum of case objects or a value type over one of them), " +
        s"and $tpeName cannot be decoded as a complex key with a YamlDecoder[$tpeName]:\n${reasons.mkString("\n")}"
  }
  final case class TransientFieldMissingDefault(fieldName: String, tpeName: String) extends DecoderDerivationError {
    override def message: String =
      s"@transientField on field '$fieldName' of $tpeName requires a default value"
  }
  final case class CannotConstructType(tpeName: String, isSingleton: Boolean, constructorError: Option[String] = None)
      extends DecoderDerivationError {
    override def message: String = {
      val prefix =
        if (isSingleton) s"Cannot construct singleton $tpeName" else s"Cannot construct $tpeName"
      constructorError.fold(prefix)(err => s"$prefix: $err")
    }
  }
}
