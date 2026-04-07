package hearth.kindlings.sconfigderivation

import hearth.fp.data.NonEmptyList

/** Error type for kindlings sconfig readers. Failures carry a path component list so
  * downstream tooling can pinpoint the offending field; multiple failures may be
  * aggregated via [[ConfigDecodingError.Multiple]].
  */
sealed trait ConfigDecodingError extends Throwable {
  def message: String
  def path: List[String]
  override def getMessage: String = {
    val pathStr = if (path.isEmpty) "" else s" (at ${path.mkString(".")})"
    s"$message$pathStr"
  }
  def withParentPath(parent: String): ConfigDecodingError
}
object ConfigDecodingError {
  final case class Missing(path: List[String], key: String) extends ConfigDecodingError {
    override def message: String = s"Missing required key '$key'"
    override def withParentPath(parent: String): ConfigDecodingError = copy(path = parent :: path)
  }
  final case class WrongType(path: List[String], expected: String, actual: String) extends ConfigDecodingError {
    override def message: String = s"Expected $expected but got $actual"
    override def withParentPath(parent: String): ConfigDecodingError = copy(path = parent :: path)
  }
  final case class CannotConvert(path: List[String], message: String, cause: Option[Throwable] = None)
      extends ConfigDecodingError {
    override def withParentPath(parent: String): ConfigDecodingError = copy(path = parent :: path)
  }
  /** Strict-mode failure: the input HOCON object had a key that's not part of the case
    * class being decoded, and `allowUnknownKeys = false` was active (matches PureConfig's
    * `pureconfig.error.UnknownKey`).
    */
  final case class UnknownKey(path: List[String], key: String) extends ConfigDecodingError {
    override def message: String = s"Unknown key '$key'"
    override def withParentPath(parent: String): ConfigDecodingError = copy(path = parent :: path)
  }
  final case class Multiple(errors: NonEmptyList[ConfigDecodingError]) extends ConfigDecodingError {
    override def message: String = errors.toList.map(_.getMessage).mkString("; ")
    override def path: List[String] = Nil
    override def withParentPath(parent: String): ConfigDecodingError =
      Multiple(NonEmptyList(errors.head.withParentPath(parent), errors.tail.map(_.withParentPath(parent))))
  }

  def merge(left: ConfigDecodingError, right: ConfigDecodingError): ConfigDecodingError =
    (left, right) match {
      case (Multiple(ls), Multiple(rs)) => Multiple(NonEmptyList(ls.head, ls.tail ++ rs.toList))
      case (Multiple(ls), r)            => Multiple(NonEmptyList(ls.head, ls.tail :+ r))
      case (l, Multiple(rs))            => Multiple(NonEmptyList(l, rs.toList))
      case (l, r)                       => Multiple(NonEmptyList(l, List(r)))
    }
}
