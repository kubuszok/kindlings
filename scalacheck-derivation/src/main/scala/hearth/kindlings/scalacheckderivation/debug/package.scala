package hearth.kindlings.scalacheckderivation

package object debug {

  /** Import this value to enable debug logging for ScalaCheck derivation.
    *
    * {{{
    * import hearth.kindlings.scalacheckderivation.debug.logDerivationForScalaCheckDerivation
    * }}}
    *
    * Alternatively, use scalac option: `-Xmacro-settings:scalacheckDerivation.logDerivation=true`
    */
  implicit val logDerivationForScalaCheckDerivation: LogDerivation = new LogDerivation
}
