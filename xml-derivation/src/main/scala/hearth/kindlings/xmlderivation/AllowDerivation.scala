package hearth.kindlings.xmlderivation

/** Opt-in marker for the derivation policy (issue kubuszok/kindlings#85). Importing
  * hearth.kindlings.xmlderivation.policy.allowDerivationForXmlDerivation permits structural derivation in the current
  * scope under `opt-in` policy with `optInByImport=true`. Kept OUTSIDE implicit scope so it is never summoned
  * automatically.
  */
sealed trait AllowDerivation
object AllowDerivation extends AllowDerivation
