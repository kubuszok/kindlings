package hearth.kindlings.diffderivation.internal.compiletime

sealed private[compiletime] trait DiffDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}

private[compiletime] object DiffDerivationError {

  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends DiffDerivationError {
    override def message: String = s"$tpeName not handled by any Diff rule:\n${reasons.mkString("\n")}"
  }
}
