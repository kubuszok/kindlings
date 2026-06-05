package hearth.kindlings.catstaglessderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait InvariantKUseCachedRuleImpl {
  this: InvariantKMacrosImpl & MacroCommons & StdExtensions =>

  object InvariantKUseCachedRule extends InvariantKDerivationRule("use cached InvariantK") {
    def apply[Alg[_[_]]: InvariantKCtx]: MIO[Rule.Applicability[Expr[cats.tagless.InvariantK[Alg]]]] = {
      implicit val IKAlgType: Type[cats.tagless.InvariantK[Alg]] = ikctx.invariantKAlgType
      ikctx.cache.get0Ary[cats.tagless.InvariantK[Alg]]("cached-invariantk-instance").flatMap {
        case Some(instance) =>
          MIO.pure(Rule.matched(instance))
        case None =>
          MIO.pure(Rule.yielded(s"No cached InvariantK for ${ikctx.invariantKAlgType.prettyPrint}"))
      }
    }
  }
}
