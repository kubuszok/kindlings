package hearth.kindlings.sconfigderivation.internal.compiletime

import hearth.MacroCommons

trait SconfigDerivationPolicy extends hearth.kindlings.derivation.compiletime.DerivationPolicy { this: MacroCommons =>
  override protected def derivationOptInImportHint: String =
    "import hearth.kindlings.sconfigderivation.policy.allowDerivationForSconfigDerivation"
  override protected def isDerivationOptInMarkerInScope: Boolean = {
    implicit val AllowDerivationT: Type[hearth.kindlings.sconfigderivation.AllowDerivation] =
      Type.of[hearth.kindlings.sconfigderivation.AllowDerivation]
    Expr.summonImplicit[hearth.kindlings.sconfigderivation.AllowDerivation].isDefined
  }
}
