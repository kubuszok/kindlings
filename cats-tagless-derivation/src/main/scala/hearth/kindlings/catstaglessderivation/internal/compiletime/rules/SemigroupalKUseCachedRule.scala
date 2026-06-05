package hearth.kindlings.catstaglessderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait SemigroupalKUseCachedRuleImpl {
  this: SemigroupalKMacrosImpl & MacroCommons & StdExtensions =>

  object SemigroupalKUseCachedRule extends SemigroupalKDerivationRule("use cached SemigroupalK") {
    def apply[Alg[_[_]]: SemigroupalKCtx]: MIO[Rule.Applicability[Expr[cats.tagless.SemigroupalK[Alg]]]] = {
      implicit val SKAlgType: Type[cats.tagless.SemigroupalK[Alg]] = skctx.semigroupalKAlgType
      skctx.cache.get0Ary[cats.tagless.SemigroupalK[Alg]]("cached-semigroupalk-instance").flatMap {
        case Some(instance) =>
          MIO.pure(Rule.matched(instance))
        case None =>
          MIO.pure(Rule.yielded(s"No cached SemigroupalK for ${skctx.semigroupalKAlgType.prettyPrint}"))
      }
    }
  }
}
