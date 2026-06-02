package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait ShowPrettyUseImplicitRuleImpl {
  this: ShowPrettyMacrosImpl & MacroCommons & StdExtensions & StrictDerivationSupport =>

  @scala.annotation.nowarn("msg=is never used")
  object ShowPrettyUseImplicitRule extends ShowDerivationRule("use implicit ShowPretty/Show") {

    def apply[A: ShowCtx]: MIO[Rule.Applicability[Expr[String]]] = {
      implicit val ShowA: Type[cats.Show[A]] = ShowTypes.Show[A]
      Log.info(s"Searching implicit ShowPretty/Show[${Type[A].prettyPrint}]") >> {
        if (sctx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is the self-type, skipping"))
        else {
          implicit val ShowPrettyA: Type[hearth.kindlings.catsderivation.ShowPretty[A]] =
            ShowPrettyTypes.ShowPretty[A]
          ShowPrettyTypes.ShowPretty[A].summonExprIgnoring().toEither match {
            case Right(instanceExpr) =>
              Log.info(s"Found implicit ShowPretty[${Type[A].prettyPrint}]") >>
                sctx.cache.buildCachedWith(
                  "cached-show-pretty-instance",
                  ValDefBuilder.ofLazy[cats.Show[A]](s"showPretty_${Type[A].shortName}")
                )(_ => instanceExpr.upcast[cats.Show[A]]) >>
                ShowPrettyUseCachedRule[A]
            case Left(_) =>
              ShowTypes.Show[A].summonExprIgnoring().toEither match {
                case Right(instanceExpr) =>
                  Log.info(s"Found implicit Show[${Type[A].prettyPrint}] (fallback)") >>
                    sctx.cache.buildCachedWith(
                      "cached-show-pretty-instance",
                      ValDefBuilder.ofLazy[cats.Show[A]](s"showPretty_${Type[A].shortName}")
                    )(_ => instanceExpr) >>
                    ShowPrettyUseCachedRule[A]
                case Left(reason) =>
                  if (isStrictDerivationMode)
                    MIO.fail(
                      new StrictDerivationError(
                        s"Strict derivation mode: no implicit ShowPretty/Show[${Type[A].prettyPrint}] found. Provide an explicit instance or remove StrictDerivation from scope."
                      )
                    )
                  else
                    MIO.pure(Rule.yielded(s"No implicit ShowPretty/Show[${Type[A].prettyPrint}]: $reason"))
              }
          }
        }
      }
    }
  }
}
