package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons

/** Module-level wiring for the derivation policy (issue kubuszok/kindlings#85). All three members are concrete (the
  * type-class name is generic) so the trait can be mixed into the heavily-composed cats impl hierarchy (e.g.
  * `Monoid extends Semigroup`, `Hash extends Eq`) without override/diamond conflicts.
  */
// $COVERAGE-OFF$ macro-only (compile-time) policy glue
trait CatsDerivationPolicy extends hearth.kindlings.derivation.compiletime.DerivationPolicy { this: MacroCommons =>

  override protected def derivationPolicyTypeClassName: String = "Cats"

  override protected def derivationOptInImportHint: String =
    "import hearth.kindlings.catsderivation.policy.allowDerivationForCatsDerivation"

  override protected def isDerivationOptInMarkerInScope: Boolean = {
    implicit val AllowDerivationT: Type[hearth.kindlings.catsderivation.AllowDerivation] =
      Type.of[hearth.kindlings.catsderivation.AllowDerivation]
    Expr.summonImplicit[hearth.kindlings.catsderivation.AllowDerivation].isDefined
  }
}
// $COVERAGE-ON$
