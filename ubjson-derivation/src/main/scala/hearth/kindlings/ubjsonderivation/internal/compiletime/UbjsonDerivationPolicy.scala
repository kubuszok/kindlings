package hearth.kindlings.ubjsonderivation.internal.compiletime

import hearth.MacroCommons

// $COVERAGE-OFF$ macro-only (compile-time) policy glue
trait UbjsonDerivationPolicy extends hearth.kindlings.derivation.compiletime.DerivationPolicy { this: MacroCommons =>
  override protected def derivationOptInImportHint: String =
    "import hearth.kindlings.ubjsonderivation.policy.allowDerivationForUbjsonDerivation"
  override protected def isDerivationOptInMarkerInScope: Boolean = {
    implicit val AllowDerivationT: Type[hearth.kindlings.ubjsonderivation.AllowDerivation] =
      Type.of[hearth.kindlings.ubjsonderivation.AllowDerivation]
    Expr.summonImplicit[hearth.kindlings.ubjsonderivation.AllowDerivation].isDefined
  }
}
// $COVERAGE-ON$
