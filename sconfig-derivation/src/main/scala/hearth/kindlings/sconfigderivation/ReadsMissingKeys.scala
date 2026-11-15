package hearth.kindlings.sconfigderivation

/** Marker for a [[ConfigReader]] that reads missing keys (matches PureConfig's `pureconfig.ReadsMissingKeys`): when the
  * key of a case-class field is missing, derived readers call such a reader with `null` instead of failing with
  * [[ConfigDecodingError.KeyNotFound]]. The built-in `Option` reader is one (a missing key reads as `None`).
  */
trait ReadsMissingKeys { this: ConfigReader[?] => }
