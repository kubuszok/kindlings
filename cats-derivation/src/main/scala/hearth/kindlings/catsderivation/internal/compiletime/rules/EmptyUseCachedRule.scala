package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait EmptyUseCachedRuleImpl {
  this: EmptyMacrosImpl & MacroCommons & StdExtensions =>

  object EmptyUseCachedRule extends EmptyDerivationRule("use cached Empty") {

    def apply[A: EmptyCtx]: MIO[Rule.Applicability[Expr[A]]] = {
      implicit val EmptyA: Type[alleycats.Empty[A]] = EmptyTypes.Empty[A]
      ectx.cache.get0Ary[alleycats.Empty[A]]("cached-empty-instance").flatMap {
        case Some(instance) =>
          MIO.pure(Rule.matched(Expr.quote(Expr.splice(instance).empty)))
        case None =>
          ectx.cache.get0Ary[A]("cached-empty-value").flatMap {
            case Some(value) =>
              MIO.pure(Rule.matched(value))
            case None =>
              MIO.pure(Rule.yielded(s"No cached Empty for ${Type[A].prettyPrint}"))
          }
      }
    }
  }
}
