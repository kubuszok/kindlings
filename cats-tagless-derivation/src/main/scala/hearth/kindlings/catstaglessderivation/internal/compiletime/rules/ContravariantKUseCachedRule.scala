package hearth.kindlings.catstaglessderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait ContravariantKUseCachedRuleImpl {
  this: ContravariantKMacrosImpl & MacroCommons & StdExtensions =>

  object ContravariantKUseCachedRule extends ContravariantKDerivationRule("use cached ContravariantK") {
    def apply[Alg[_[_]]: ContravariantKCtx]: MIO[Rule.Applicability[Expr[cats.tagless.ContravariantK[Alg]]]] = {
      implicit val CKAlgType: Type[cats.tagless.ContravariantK[Alg]] = ckctx.contravariantKAlgType
      ckctx.cache.get0Ary[cats.tagless.ContravariantK[Alg]]("cached-contravariantk-instance").flatMap {
        case Some(instance) =>
          MIO.pure(Rule.matched(instance))
        case None =>
          MIO.pure(Rule.yielded(s"No cached ContravariantK for ${ckctx.contravariantKAlgType.prettyPrint}"))
      }
    }
  }
}
