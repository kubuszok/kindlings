package hearth.kindlings.sconfigderivation.internal.compiletime

sealed private[compiletime] trait ReaderDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object ReaderDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends ReaderDerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any reader derivation rule:\n${reasons.mkString("\n")}"
  }
  final case class TransientFieldMissingDefault(fieldName: String, tpeName: String) extends ReaderDerivationError {
    override def message: String =
      s"@transientField on field '$fieldName' of $tpeName requires a default value"
  }
  final case class CannotConstructType(tpeName: String, isSingleton: Boolean, constructorError: Option[String] = None)
      extends ReaderDerivationError {
    override def message: String = {
      val prefix =
        if (isSingleton) s"Cannot construct singleton $tpeName" else s"Cannot construct $tpeName"
      constructorError.fold(prefix)(err => s"$prefix: $err")
    }
  }
}
