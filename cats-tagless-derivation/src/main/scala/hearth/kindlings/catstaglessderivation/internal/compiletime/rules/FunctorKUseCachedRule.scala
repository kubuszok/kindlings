package hearth.kindlings.catstaglessderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait FunctorKUseCachedRuleImpl {
  this: FunctorKMacrosImpl & MacroCommons & StdExtensions =>

  object FunctorKUseCachedRule extends FunctorKDerivationRule("use cached FunctorK") {
    def apply[Alg[_[_]]: FunctorKCtx]: MIO[Rule.Applicability[Expr[cats.tagless.FunctorK[Alg]]]] = {
      implicit val FKAlgType: Type[cats.tagless.FunctorK[Alg]] = fkctx.functorKAlgType
      fkctx.cache.get0Ary[cats.tagless.FunctorK[Alg]]("cached-functork-instance").flatMap {
        case Some(instance) =>
          MIO.pure(Rule.matched(instance))
        case None =>
          MIO.pure(Rule.yielded(s"No cached FunctorK for ${fkctx.functorKAlgType.prettyPrint}"))
      }
    }
  }
}
