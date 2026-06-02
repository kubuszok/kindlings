package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait ShowUseImplicitRuleImpl {
  this: ShowMacrosImpl & MacroCommons & StdExtensions & StrictDerivationSupport =>

  object ShowUseImplicitRule extends ShowDerivationRule("use implicit Show") {

    def apply[A: ShowCtx]: MIO[Rule.Applicability[Expr[String]]] = {
      implicit val ShowA: Type[cats.Show[A]] = ShowTypes.Show[A]
      Log.info(s"Searching implicit Show[${Type[A].prettyPrint}]") >> {
        if (sctx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is the self-type, skipping"))
        else
          ShowTypes.Show[A].summonExprIgnoring().toEither match {
            case Right(instanceExpr) =>
              Log.info(s"Found implicit Show[${Type[A].prettyPrint}]: ${instanceExpr.prettyPrint}") >>
                sctx.cache.buildCachedWith(
                  "cached-show-instance",
                  ValDefBuilder.ofLazy[cats.Show[A]](s"show_${Type[A].shortName}")
                )(_ => instanceExpr) >>
                ShowUseCachedRule[A]
            case Left(reason) =>
              if (isStrictDerivationMode)
                MIO.fail(
                  new StrictDerivationError(
                    s"Strict derivation mode: no implicit Show[${Type[A].prettyPrint}] found. Provide an explicit instance or remove StrictDerivation from scope."
                  )
                )
              else
                MIO.pure(Rule.yielded(s"No implicit Show[${Type[A].prettyPrint}]: $reason"))
          }
      }
    }
  }
}
