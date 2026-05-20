package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait ShowPrettyMacrosImpl
    extends ShowMacrosImpl
    with rules.ShowPrettyUseCachedRuleImpl
    with rules.ShowPrettyUseImplicitRuleImpl
    with rules.ShowPrettyCaseClassRuleImpl
    with rules.ShowPrettyEnumRuleImpl { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  def deriveShowPretty[A: Type]: Expr[hearth.kindlings.catsderivation.ShowPretty[A]] = {
    val macroName = "ShowPretty.derived"
    implicit val ShowPrettyA: Type[hearth.kindlings.catsderivation.ShowPretty[A]] = ShowPrettyTypes.ShowPretty[A]
    implicit val StringType: Type[String] = ShowTypes.String
    val selfType: Option[??] = Some(Type[A].as_??)

    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended."
      )

    Log
      .namedScope(s"Deriving ShowPretty[${Type[A].prettyPrint}] at: ${Environment.currentPosition.prettyPrint}") {
        MIO.scoped { runSafe =>
          val fromCtx: ShowCtx[A] => Expr[String] = (ctx: ShowCtx[A]) =>
            runSafe {
              for {
                _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
                result <- deriveShowPrettyRecursively[A](using ctx)
                cache <- ctx.cache.get
              } yield cache.toValDefs.use(_ => result)
            }

          Expr.quote {
            hearth.kindlings.catsderivation.internal.runtime.CatsDerivationFactories
              .showPrettyInstance[A] { (value: A) =>
                val _ = value
                Expr.splice {
                  fromCtx(ShowCtx.from(Expr.quote(value), derivedType = selfType))
                }
              }
          }
        }
      }
      .flatTap(result => Log.info(s"Derived final result: ${result.prettyPrint}"))
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogShowDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogShowDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      ) { (errorLogs, errors) =>
        val errorsRendered = errors.map(e => "  - " + e.getMessage).mkString("\n")
        val hint =
          "Enable debug logging with: import hearth.kindlings.catsderivation.debug.logDerivationForCatsDerivation"
        if (errorLogs.nonEmpty) s"Macro derivation failed:\n$errorsRendered\nlogs:\n$errorLogs\n$hint"
        else s"Macro derivation failed:\n$errorsRendered\n$hint"
      }
  }

  def deriveShowPrettyRecursively[A: ShowCtx]: MIO[Expr[String]] =
    Log.namedScope(s"Deriving ShowPretty for ${Type[A].prettyPrint}") {
      Rules(
        ShowPrettyUseCachedRule,
        ShowPrettyUseImplicitRule,
        ShowBuiltInRule,
        ShowValueTypeRule,
        ShowOptionRule,
        ShowMapRule,
        ShowCollectionRule,
        ShowSingletonRule,
        ShowPrettyCaseClassRule,
        ShowPrettyEnumRule
      )(_[A]).flatMap {
        case Right(result) =>
          Log.info(s"Derived ShowPretty for ${Type[A].prettyPrint}") >> MIO.pure(result)
        case Left(reasons) =>
          val reasonsStrings = reasons.toListMap
            .removed(ShowPrettyUseCachedRule)
            .view
            .map { case (rule, reasons) =>
              if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
              else s" - ${rule.name}: ${reasons.mkString(", ")}"
            }
            .toList
          val err = ShowDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
          Log.error(err.message) >> MIO.fail(err)
      }
    }

  protected object ShowPrettyTypes {
    def ShowPretty: Type.Ctor1[hearth.kindlings.catsderivation.ShowPretty] =
      Type.Ctor1.of[hearth.kindlings.catsderivation.ShowPretty]
  }
}
