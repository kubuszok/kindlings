package hearth.kindlings.catstaglessderivation.internal.compiletime

import hearth.MacroCommons

/** Module-level wiring for the derivation policy (issue kubuszok/kindlings#85). All members concrete (generic
  * type-class name) so it can be mixed into the composed K-type-class impl hierarchy (e.g. `ApplyK extends FunctorK
  * with SemigroupalK`) without override/diamond conflicts.
  */
trait CatsTaglessDerivationPolicy extends hearth.kindlings.derivation.compiletime.DerivationPolicy {
  this: MacroCommons =>

  override protected def derivationPolicyTypeClassName: String = "CatsTagless"

  override protected def derivationOptInImportHint: String =
    "import hearth.kindlings.catstaglessderivation.policy.allowDerivationForCatsTaglessDerivation"

  override protected def isDerivationOptInMarkerInScope: Boolean = {
    implicit val AllowDerivationT: Type[hearth.kindlings.catstaglessderivation.AllowDerivation] =
      Type.of[hearth.kindlings.catstaglessderivation.AllowDerivation]
    Expr.summonImplicit[hearth.kindlings.catstaglessderivation.AllowDerivation].isDefined
  }
}
