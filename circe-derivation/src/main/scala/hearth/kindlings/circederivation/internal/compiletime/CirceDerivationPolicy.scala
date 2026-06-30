package hearth.kindlings.circederivation.internal.compiletime

import hearth.MacroCommons

/** Module-level wiring for the derivation policy (issue kubuszok/kindlings#85). Provides the Circe opt-in marker check
  * and import hint; each `*MacrosImpl` mixes this in (alongside [[CirceDerivationTimeout]]) and only supplies its own
  * `derivationPolicyTypeClassName`.
  */
// $COVERAGE-OFF$ macro-only (compile-time) policy glue
trait CirceDerivationPolicy extends hearth.kindlings.derivation.compiletime.DerivationPolicy { this: MacroCommons =>

  override protected def derivationOptInImportHint: String =
    "import hearth.kindlings.circederivation.policy.allowDerivationForCirceDerivation"

  override protected def isDerivationOptInMarkerInScope: Boolean = {
    implicit val AllowDerivationT: Type[hearth.kindlings.circederivation.AllowDerivation] =
      Type.of[hearth.kindlings.circederivation.AllowDerivation]
    Expr.summonImplicit[hearth.kindlings.circederivation.AllowDerivation].isDefined
  }
}
// $COVERAGE-ON$
