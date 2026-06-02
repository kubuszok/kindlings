package hearth.kindlings.catsderivation

/** When an implicit `StrictDerivation` is in scope, derivation macros will fail at compile time if any transitive field
  * type class instance would need to be auto-derived. All field instances must be explicitly provided by the user.
  *
  * Usage:
  * {{{
  * implicit val strict: StrictDerivation = StrictDerivation.instance
  * val show = Show.derived[Outer]  // fails if Show[Inner] is not in scope
  * }}}
  */
sealed trait StrictDerivation
object StrictDerivation {
  val instance: StrictDerivation = new StrictDerivation {}
}
