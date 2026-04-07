package hearth.kindlings.pureconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.pureconfigderivation.PureConfig
import pureconfig.{ConfigCursor, ConfigReader}
import pureconfig.error.ConfigReaderFailures

trait ReaderUseCachedDefWhenAvailableRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object ReaderUseCachedDefWhenAvailableRule extends ReaderDerivationRule("use cached def when available") {

    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigReaderFailures, A]]]] =
      Log.info(s"Attempting to use cached reader for ${Type[A].prettyPrint}") >>
        rctx.getInstance[A].flatMap {
          case Some(instance) => callCachedInstance[A](instance)
          case None           =>
            rctx.getHelper[A].flatMap {
              case Some(helperCall) => callCachedHelper[A](helperCall)
              case None             => yieldUnsupported[A]
            }
        }

    private def callCachedInstance[A: ReaderCtx](
        instance: Expr[ConfigReader[A]]
    ): MIO[Rule.Applicability[Expr[Either[ConfigReaderFailures, A]]]] =
      Log.info(s"Found cached reader instance for ${Type[A].prettyPrint}") >> MIO.pure(
        Rule.matched(Expr.quote {
          Expr.splice(instance).from(Expr.splice(rctx.cursor))
        })
      )

    private def callCachedHelper[A: ReaderCtx](
        helperCall: (Expr[ConfigCursor], Expr[PureConfig]) => Expr[Either[ConfigReaderFailures, A]]
    ): MIO[Rule.Applicability[Expr[Either[ConfigReaderFailures, A]]]] =
      Log.info(s"Found cached reader helper for ${Type[A].prettyPrint}") >> MIO.pure(
        Rule.matched(helperCall(rctx.cursor, rctx.config))
      )

    private def yieldUnsupported[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigReaderFailures, A]]]] =
      MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached reader"))
  }
}
