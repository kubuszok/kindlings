package hearth.kindlings.jsoniterderivation.internal.compiletime

import hearth.MacroCommons

// $COVERAGE-OFF$
trait JsoniterDerivationPolicy extends hearth.kindlings.derivation.compiletime.DerivationPolicy { this: MacroCommons =>
  override protected def derivationOptInImportHint: String =
    "import hearth.kindlings.jsoniterderivation.policy.allowDerivationForJsoniterDerivation"
  override protected def isDerivationOptInMarkerInScope: Boolean = {
    implicit val AllowDerivationT: Type[hearth.kindlings.jsoniterderivation.AllowDerivation] =
      Type.of[hearth.kindlings.jsoniterderivation.AllowDerivation]
    Expr.summonImplicit[hearth.kindlings.jsoniterderivation.AllowDerivation].isDefined
  }
}
// $COVERAGE-ON$
