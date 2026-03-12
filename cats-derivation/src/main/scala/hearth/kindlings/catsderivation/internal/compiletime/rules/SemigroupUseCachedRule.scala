package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait SemigroupUseCachedRuleImpl {
  this: SemigroupMacrosImpl & MacroCommons & StdExtensions =>

  object SemigroupUseCachedRule extends SemigroupDerivationRule("use cached Semigroup") {

    def apply[A: SemigroupCtx]: MIO[Rule.Applicability[Expr[A]]] = {
      implicit val SemigroupA: Type[cats.kernel.Semigroup[A]] = SemigroupTypes.Semigroup[A]
      sgctx.cache.get0Ary[cats.kernel.Semigroup[A]]("cached-semigroup-instance").flatMap {
        case Some(instance) =>
          MIO.pure(
            Rule.matched(
              Expr.quote(Expr.splice(instance).combine(Expr.splice(sgctx.x), Expr.splice(sgctx.y)))
            )
          )
        case None =>
          sgctx.cache.get2Ary[A, A, A]("cached-semigroup-method").flatMap {
            case Some(helper) =>
              MIO.pure(Rule.matched(helper(sgctx.x, sgctx.y)))
            case None =>
              MIO.pure(Rule.yielded(s"No cached Semigroup for ${Type[A].prettyPrint}"))
          }
      }
    }
  }
}
