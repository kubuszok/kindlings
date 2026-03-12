package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait HashUseCachedRuleImpl {
  this: HashMacrosImpl & MacroCommons & StdExtensions =>

  object HashUseCachedRule extends HashDerivationRule("use cached Hash") {

    def apply[A: HashCtx]: MIO[Rule.Applicability[Expr[Int]]] = {
      implicit val HashA: Type[cats.kernel.Hash[A]] = HashTypes.Hash[A]
      implicit val IntType: Type[Int] = HashTypes.Int
      hctx.cache.get0Ary[cats.kernel.Hash[A]]("cached-hash-instance").flatMap {
        case Some(instance) =>
          MIO.pure(Rule.matched(Expr.quote(Expr.splice(instance).hash(Expr.splice(hctx.value)))))
        case None =>
          hctx.cache.get1Ary[A, Int]("cached-hash-method").flatMap {
            case Some(helper) =>
              MIO.pure(Rule.matched(helper(hctx.value)))
            case None =>
              MIO.pure(Rule.yielded(s"No cached Hash for ${Type[A].prettyPrint}"))
          }
      }
    }
  }
}
