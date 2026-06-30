package hearth.kindlings.scalacheckderivation.internal.compiletime

import hearth.MacroCommons

trait ScalacheckDerivationPolicy extends hearth.kindlings.derivation.compiletime.DerivationPolicy {
  this: MacroCommons =>
  override protected def derivationOptInImportHint: String =
    "import hearth.kindlings.scalacheckderivation.policy.allowDerivationForScalacheckDerivation"
  override protected def isDerivationOptInMarkerInScope: Boolean = {
    implicit val AllowDerivationT: Type[hearth.kindlings.scalacheckderivation.AllowDerivation] =
      Type.of[hearth.kindlings.scalacheckderivation.AllowDerivation]
    Expr.summonImplicit[hearth.kindlings.scalacheckderivation.AllowDerivation].isDefined
  }
}
