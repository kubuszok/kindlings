package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait ShowPrettyUseCachedRuleImpl {
  this: ShowPrettyMacrosImpl & MacroCommons & StdExtensions =>

  object ShowPrettyUseCachedRule extends ShowDerivationRule("use cached ShowPretty") {

    def apply[A: ShowCtx]: MIO[Rule.Applicability[Expr[String]]] = {
      implicit val ShowA: Type[cats.Show[A]] = ShowTypes.Show[A]
      sctx.cache.get0Ary[cats.Show[A]]("cached-show-pretty-instance").flatMap {
        case Some(instance) =>
          Log.info(s"Using cached ShowPretty instance for ${Type[A].prettyPrint}") >>
            MIO.pure(Rule.matched(Expr.quote(Expr.splice(instance).show(Expr.splice(sctx.value)))))
        case None =>
          implicit val StringType: Type[String] = ShowTypes.String
          sctx.cache.get1Ary[A, String]("cached-show-pretty-method").flatMap {
            case Some(helper) =>
              Log.info(s"Using cached ShowPretty helper for ${Type[A].prettyPrint}") >>
                MIO.pure(Rule.matched(helper(sctx.value)))
            case None =>
              MIO.pure(Rule.yielded(s"No cached definition for ${Type[A].prettyPrint}"))
          }
      }
    }
  }
}
