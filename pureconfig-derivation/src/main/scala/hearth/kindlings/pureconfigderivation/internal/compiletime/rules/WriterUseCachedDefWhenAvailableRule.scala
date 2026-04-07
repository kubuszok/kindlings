package hearth.kindlings.pureconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import com.typesafe.config.ConfigValue
import hearth.kindlings.pureconfigderivation.PureConfig
import pureconfig.ConfigWriter

trait WriterUseCachedDefWhenAvailableRuleImpl {
  this: WriterMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object WriterUseCachedDefWhenAvailableRule extends WriterDerivationRule("use cached def when available") {

    def apply[A: WriterCtx]: MIO[Rule.Applicability[Expr[ConfigValue]]] =
      Log.info(s"Attempting to use cached writer for ${Type[A].prettyPrint}") >>
        wctx.getInstance[A].flatMap {
          case Some(instance) => callCachedInstance[A](instance)
          case None           =>
            wctx.getHelper[A].flatMap {
              case Some(helperCall) => callCachedHelper[A](helperCall)
              case None             => yieldUnsupported[A]
            }
        }

    private def callCachedInstance[A: WriterCtx](
        instance: Expr[ConfigWriter[A]]
    ): MIO[Rule.Applicability[Expr[ConfigValue]]] =
      Log.info(s"Found cached writer instance for ${Type[A].prettyPrint}") >> MIO.pure(
        Rule.matched(Expr.quote {
          Expr.splice(instance).to(Expr.splice(wctx.value))
        })
      )

    private def callCachedHelper[A: WriterCtx](
        helperCall: (Expr[A], Expr[PureConfig]) => Expr[ConfigValue]
    ): MIO[Rule.Applicability[Expr[ConfigValue]]] =
      Log.info(s"Found cached writer helper for ${Type[A].prettyPrint}") >> MIO.pure(
        Rule.matched(helperCall(wctx.value, wctx.config))
      )

    private def yieldUnsupported[A: WriterCtx]: MIO[Rule.Applicability[Expr[ConfigValue]]] =
      MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached writer"))
  }
}
