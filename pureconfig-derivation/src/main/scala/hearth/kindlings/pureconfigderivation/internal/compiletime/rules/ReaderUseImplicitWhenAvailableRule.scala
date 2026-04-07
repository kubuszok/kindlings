package hearth.kindlings.pureconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.pureconfigderivation.{KindlingsConfigConvert, KindlingsConfigReader}
import pureconfig.ConfigReader
import pureconfig.error.ConfigReaderFailures

trait ReaderUseImplicitWhenAvailableRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object ReaderUseImplicitWhenAvailableRule extends ReaderDerivationRule("use implicit when available") {

    /** Methods to exclude from `summonExprIgnoring` so that the macro never finds and reuses
      * its own auto-derivation entry points (which would cause infinite expansion).
      *
      * **Important**: this filter only covers our own `KindlingsConfigReader.derived` /
      * `KindlingsConfigConvert.derived` companion methods. PureConfig also ships several
      * auto-derivation surfaces of its own — `pureconfig.generic.auto._` (Scala 2,
      * Shapeless-based) and `pureconfig.generic.derivation.default._` (Scala 3 native
      * `derives`). Users who want **kindlings derivation** should NOT also import those
      * pureconfig packages, otherwise pureconfig's implicit will win during
      * `summonExpr` and our rule chain will never run.
      *
      * If a user imports both, the resulting `ConfigReader[A]` will be the one provided
      * by whichever import is "more specific" in implicit-resolution rules (typically
      * pureconfig's), which will silently bypass kindlings annotations / config knobs.
      */
    lazy val ignoredImplicits: Seq[UntypedMethod] =
      Type.of[KindlingsConfigReader.type].methods.collect {
        case method if method.value.isImplicit => method.value.asUntyped
      } ++ Type.of[KindlingsConfigConvert.type].methods.collect {
        case method if method.value.isImplicit => method.value.asUntyped
      }

    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigReaderFailures, A]]]] =
      Log.info(s"Attempting to use implicit ConfigReader for ${Type[A].prettyPrint}") >> {
        if (rctx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          RTypes.ConfigReader[A].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(instanceExpr) => cacheAndUse[A](instanceExpr)
            case Left(reason)        => yieldUnsupported[A](reason)
          }
      }

    private def cacheAndUse[A: ReaderCtx](
        instanceExpr: Expr[ConfigReader[A]]
    ): MIO[Rule.Applicability[Expr[Either[ConfigReaderFailures, A]]]] =
      Log.info(s"Found implicit reader ${instanceExpr.prettyPrint}, using directly") >>
        MIO.pure(Rule.matched(Expr.quote {
          Expr.splice(instanceExpr).from(Expr.splice(rctx.cursor))
        }))

    private def yieldUnsupported[A: ReaderCtx](
        reason: String
    ): MIO[Rule.Applicability[Expr[Either[ConfigReaderFailures, A]]]] =
      MIO.pure(
        Rule.yielded(
          s"The type ${Type[A].prettyPrint} does not have an implicit ConfigReader instance: $reason"
        )
      )
  }
}
