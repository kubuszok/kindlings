package hearth.kindlings.diffderivation.internal.compiletime

import hearth.MacroCommons

// $COVERAGE-OFF$ macro-only (compile-time) policy glue
trait DiffDerivationPolicy extends hearth.kindlings.derivation.compiletime.DerivationPolicy { this: MacroCommons =>
  override protected def derivationOptInImportHint: String =
    "import hearth.kindlings.diffderivation.policy.allowDerivationForDiffDerivation"
  override protected def isDerivationOptInMarkerInScope: Boolean = {
    implicit val AllowDerivationT: Type[hearth.kindlings.diffderivation.AllowDerivation] =
      Type.of[hearth.kindlings.diffderivation.AllowDerivation]
    Expr.summonImplicit[hearth.kindlings.diffderivation.AllowDerivation].isDefined
  }
}
// $COVERAGE-ON$
