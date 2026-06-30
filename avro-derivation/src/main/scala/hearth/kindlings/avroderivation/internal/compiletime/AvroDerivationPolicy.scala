package hearth.kindlings.avroderivation.internal.compiletime

import hearth.MacroCommons

// $COVERAGE-OFF$
trait AvroDerivationPolicy extends hearth.kindlings.derivation.compiletime.DerivationPolicy { this: MacroCommons =>
  override protected def derivationOptInImportHint: String =
    "import hearth.kindlings.avroderivation.policy.allowDerivationForAvroDerivation"
  override protected def isDerivationOptInMarkerInScope: Boolean = {
    implicit val AllowDerivationT: Type[hearth.kindlings.avroderivation.AllowDerivation] =
      Type.of[hearth.kindlings.avroderivation.AllowDerivation]
    Expr.summonImplicit[hearth.kindlings.avroderivation.AllowDerivation].isDefined
  }
}
// $COVERAGE-ON$
