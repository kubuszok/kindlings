package hearth.kindlings.yamlderivation.internal.compiletime

sealed private[compiletime] trait EncoderDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object EncoderDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends EncoderDerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any encoder derivation rule:\n${reasons.mkString("\n")}"
  }
  final case class NoKeyEncoder(tpeName: String, reasons: List[String]) extends EncoderDerivationError {
    override def message: String =
      s"No YamlKeyEncoder[$tpeName] for the map's keys: none in scope and none built in or derivable ($tpeName is not " +
        "a String, a primitive, BigInt, BigDecimal, UUID, an enum of case objects or a value type over one of them), " +
        s"and $tpeName cannot be encoded as a complex key with a YamlEncoder[$tpeName]:\n${reasons.mkString("\n")}"
  }
  final case class TransientFieldMissingDefault(fieldName: String, tpeName: String) extends EncoderDerivationError {
    override def message: String =
      s"@transientField on field '$fieldName' of $tpeName requires a default value"
  }
  final case class NoChildrenInSealedTrait(tpeName: String) extends EncoderDerivationError {
    override def message: String =
      s"The type $tpeName does not have any children!"
  }
}
