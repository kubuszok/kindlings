package hearth.kindlings.pureconfigderivation.internal.compiletime

import hearth.MacroCommons

// $COVERAGE-OFF$
trait PureconfigDerivationPolicy extends hearth.kindlings.derivation.compiletime.DerivationPolicy {
  this: MacroCommons =>
  override protected def derivationOptInImportHint: String =
    "import hearth.kindlings.pureconfigderivation.policy.allowDerivationForPureconfigDerivation"
  override protected def isDerivationOptInMarkerInScope: Boolean = {
    implicit val AllowDerivationT: Type[hearth.kindlings.pureconfigderivation.AllowDerivation] =
      Type.of[hearth.kindlings.pureconfigderivation.AllowDerivation]
    Expr.summonImplicit[hearth.kindlings.pureconfigderivation.AllowDerivation].isDefined
  }
}
// $COVERAGE-ON$
