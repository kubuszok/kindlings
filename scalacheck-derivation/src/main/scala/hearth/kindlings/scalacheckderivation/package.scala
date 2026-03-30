package hearth.kindlings.scalacheckderivation

/** Marker type to enable debug logging for ScalaCheck derivation.
  *
  * Import [[debug.logDerivationForScalaCheckDerivation]] to enable logging, or use scalac option:
  * `-Xmacro-settings:scalacheckDerivation.logDerivation=true`
  */
final class LogDerivation
