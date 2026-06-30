package hearth.kindlings.catsderivation

/** Opt-in marker for the derivation policy (issue kubuszok/kindlings#85). Importing
  * [[hearth.kindlings.catsderivation.policy.allowDerivationForCatsDerivation]] permits structural derivation of the
  * cats/alleycats type classes in the current scope when the build runs the policy in `opt-in` mode with
  * `optInByImport=true`. Kept OUTSIDE implicit scope so it is never summoned automatically.
  */
sealed trait AllowDerivation
object AllowDerivation extends AllowDerivation
