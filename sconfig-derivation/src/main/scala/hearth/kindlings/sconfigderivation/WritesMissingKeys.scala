package hearth.kindlings.sconfigderivation

import org.ekrich.config.ConfigValue

/** A [[ConfigWriter]] that decides whether the key of a case-class field is written at all (matches PureConfig's
  * `pureconfig.WritesMissingKeys`): derived writers of case classes call [[toOpt]] and omit the key for `None`. The
  * built-in `Option` writer is one (an empty `Option` is omitted).
  */
trait WritesMissingKeys[A] { this: ConfigWriter[A] =>
  def toOpt(value: A): Option[ConfigValue]
}
