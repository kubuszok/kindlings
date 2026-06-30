package hearth.kindlings.avroderivation

/** Opt-in marker for the derivation policy (issue kubuszok/kindlings#85). Importing
  * hearth.kindlings.avroderivation.policy.allowDerivationForAvroDerivation permits structural derivation under `opt-in` policy with `optInByImport=true`.
  * Kept OUTSIDE implicit scope so it is never summoned automatically. */
sealed trait AllowDerivation
object AllowDerivation extends AllowDerivation
