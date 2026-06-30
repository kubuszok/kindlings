package hearth.kindlings.tapirschemaderivation

/** Opt-in marker for the derivation policy (issue kubuszok/kindlings#85). Importing
  * hearth.kindlings.tapirschemaderivation.policy.allowDerivationForTapirSchemaDerivation permits structural derivation under `opt-in` policy with `optInByImport=true`.
  * Kept OUTSIDE implicit scope so it is never summoned automatically. */
sealed trait AllowDerivation
object AllowDerivation extends AllowDerivation
