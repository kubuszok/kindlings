package hearth.kindlings.catsderivation

/** Special type - if its implicit is in scope then macros will log the derivation process.
  *
  * @see
  *   [[hearth.kindlings.catsderivation.debug.logDerivationForCatsDerivation]] for details
  */
sealed trait LogDerivation
object LogDerivation {
  val instance: LogDerivation = new LogDerivation {}
}
