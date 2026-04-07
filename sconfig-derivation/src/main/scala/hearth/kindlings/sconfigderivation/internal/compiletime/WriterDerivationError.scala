package hearth.kindlings.sconfigderivation.internal.compiletime

sealed private[compiletime] trait WriterDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object WriterDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends WriterDerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any writer derivation rule:\n${reasons.mkString("\n")}"
  }
  final case class TransientFieldMissingDefault(fieldName: String, tpeName: String) extends WriterDerivationError {
    override def message: String =
      s"@transientField on field '$fieldName' of $tpeName requires a default value"
  }
  final case class NoChildrenInSealedTrait(tpeName: String) extends WriterDerivationError {
    override def message: String =
      s"The type $tpeName does not have any children!"
  }
}
