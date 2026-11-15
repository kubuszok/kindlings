package hearth.kindlings.sconfigderivation

import hearth.fp.data.NonEmptyList
import org.ekrich.config.{ConfigRenderOptions, ConfigValue}

/** Error type for kindlings sconfig readers. Failures carry a path component list so downstream tooling can pinpoint
  * the offending field; multiple failures may be aggregated via [[ConfigDecodingError.Multiple]].
  *
  * Derived readers report the failures upstream PureConfig's derivation reports (`KeyNotFound`, `WrongType`,
  * `UnknownKey`, `CannotConvert`, `UnexpectedValueForFieldCoproductHint`, `NoValidCoproductOptionFound`), at the same
  * paths: a missing key at the object that lacks it, any other failure at the offending value (`path` lists the keys /
  * list indices from the value the reader was called on). They report every failure, not only the first one.
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

  /** A missing key without candidate keys. Derived readers report [[KeyNotFound]] instead (as upstream PureConfig
    * does); this failure is kept for hand-written readers.
    */
  final case class Missing(path: List[String], key: String) extends ConfigDecodingError {
    override def message: String = s"Missing required key '$key'"
    override def withParentPath(parent: String): ConfigDecodingError = copy(path = parent :: path)
  }

  /** A missing key (matches PureConfig's `pureconfig.error.KeyNotFound`), reported at the object that lacks it.
    * `candidates` are the object's keys that might be the expected key under a misconfigured naming (see
    * [[KeyNotFound.forKeys]]).
    */
  final case class KeyNotFound(path: List[String], key: String, candidates: Set[String] = Set.empty)
      extends ConfigDecodingError {
    override def message: String =
      if (candidates.isEmpty) s"Key not found: '$key'."
      else
        (s"Key not found: '$key'. You might have a misconfigured ProductHint, since the following similar keys were found:" ::
          candidates.toList.map(c => s"  - '$c'")).mkString("\n")
    override def withParentPath(parent: String): ConfigDecodingError = copy(path = parent :: path)
  }
  object KeyNotFound {

    /** The failure for a missing `key` of an object with `keys`, with upstream PureConfig's candidate keys
      * (`KeyNotFound.forKeys`): the keys that contain the letters and digits of `key` (lower-cased) as a subsequence.
      */
    def forKeys(key: String, keys: Iterable[String]): KeyNotFound = {
      @scala.annotation.tailrec
      def isSubsequence(s1: String, s2: String): Boolean =
        if (s1.isEmpty) true
        else if (s2.isEmpty) false
        else if (s1.head == s2.head) isSubsequence(s1.tail, s2.tail)
        else isSubsequence(s1, s2.tail)
      val lcKey = key.toLowerCase.filter(c => c.isDigit || c.isLetter)
      KeyNotFound(Nil, key, keys.filter(k => isSubsequence(lcKey, k.toLowerCase)).toSet)
    }
  }

  /** A value of the wrong type (matches PureConfig's `pureconfig.error.WrongType`); `expected` and `actual` are
    * `ConfigValueType` names.
    */
  final case class WrongType(path: List[String], expected: String, actual: String) extends ConfigDecodingError {
    override def message: String = s"Expected type $expected. Found $actual instead."
    override def withParentPath(parent: String): ConfigDecodingError = copy(path = parent :: path)
  }
  final case class CannotConvert(path: List[String], message: String, cause: Option[Throwable] = None)
      extends ConfigDecodingError {
    override def withParentPath(parent: String): ConfigDecodingError = copy(path = parent :: path)
  }

  /** Strict-mode failure: the input HOCON object had a key that's not part of the case class being decoded, and
    * `allowUnknownKeys = false` was active (matches PureConfig's `pureconfig.error.UnknownKey`). Derived readers report
    * it at the unknown key.
    */
  final case class UnknownKey(path: List[String], key: String) extends ConfigDecodingError {
    override def message: String = s"Unknown key '$key'."
    override def withParentPath(parent: String): ConfigDecodingError = copy(path = parent :: path)
  }

  /** The discriminator field of a sealed family's value names no subtype (matches PureConfig's
    * `UnexpectedValueForFieldCoproductHint`), reported at the discriminator field.
    */
  final case class UnexpectedValueForFieldCoproductHint(path: List[String], value: ConfigValue)
      extends ConfigDecodingError {
    override def message: String =
      s"Unexpected value ${value.render(ConfigRenderOptions.concise)} found. Note that subtype names are transformed " +
        "with the coproduct hint's (or SConfig's) transformConstructorNames, kebab-case by default."
    override def withParentPath(parent: String): ConfigDecodingError = copy(path = parent :: path)
  }

  /** No subtype of a sealed family read with [[CoproductHint.FirstSuccess]] could be read from the value (matches
    * PureConfig's `NoValidCoproductOptionFound`). `optionFailures` lists every attempted subtype, in the order
    * attempted, with its failure (whose paths are relative to this failure's path).
    */
  final case class NoValidCoproductOptionFound(
      path: List[String],
      value: ConfigValue,
      optionFailures: List[(String, ConfigDecodingError)]
  ) extends ConfigDecodingError {
    override def message: String = {
      val base = s"No valid coproduct option found for '${value.render(ConfigRenderOptions.concise)}'."
      if (optionFailures.isEmpty) base
      else
        base + "\n" + optionFailures
          .map { case (option, failure) =>
            s"Can't use coproduct option '$option':\n" +
              flatten(failure).map(f => s"  - ${f.getMessage}").mkString("\n")
          }
          .mkString("\n")
    }
    override def withParentPath(parent: String): ConfigDecodingError = copy(path = parent :: path)
  }

  /** A subtype written with a discriminator field already has a key with the discriminator's name (matches PureConfig's
    * `CollidingKeys`); derived writers throw it inside a [[CoproductHintException]].
    */
  final case class CollidingKeys(path: List[String], key: String, existingValue: ConfigValue)
      extends ConfigDecodingError {
    override def message: String =
      s"Key with value '${existingValue.render(ConfigRenderOptions.concise)}' collides with a key necessary to disambiguate a coproduct."
    override def withParentPath(parent: String): ConfigDecodingError = copy(path = parent :: path)
  }

  final case class Multiple(errors: NonEmptyList[ConfigDecodingError]) extends ConfigDecodingError {
    override def message: String = errors.toList.map(_.getMessage).mkString("; ")
    override def path: List[String] = Nil
    override def withParentPath(parent: String): ConfigDecodingError =
      Multiple(NonEmptyList(errors.head.withParentPath(parent), errors.tail.map(_.withParentPath(parent))))
  }

  /** The individual failures of `error`: the elements of a [[Multiple]] (recursively), or `error` itself. */
  def flatten(error: ConfigDecodingError): List[ConfigDecodingError] = error match {
    case Multiple(errors) => errors.toList.flatMap(flatten)
    case single           => List(single)
  }

  def merge(left: ConfigDecodingError, right: ConfigDecodingError): ConfigDecodingError =
    (left, right) match {
      case (Multiple(ls), Multiple(rs)) => Multiple(NonEmptyList(ls.head, ls.tail ++ rs.toList))
      case (Multiple(ls), r)            => Multiple(NonEmptyList(ls.head, ls.tail :+ r))
      case (l, Multiple(rs))            => Multiple(NonEmptyList(l, rs.toList))
      case (l, r)                       => Multiple(NonEmptyList(l, List(r)))
    }
}
