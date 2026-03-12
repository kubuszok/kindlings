package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.JsoniterConfig
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec}

trait DecoderUseCachedDefWhenAvailableRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderUseCachedDefWhenAvailableRule extends DecoderDerivationRule("use cached def when available") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to use cached decoder for ${Type[A].prettyPrint}") >>
        dctx.getInstance[A].flatMap {
          case Some(instance) => callCachedInstance[A](instance)
          case None           =>
            dctx.getHelper[A].flatMap {
              case Some(helperCall) => callCachedHelper[A](helperCall)
              case None             => yieldUnsupported[A]
            }
        }

    private def callCachedInstance[A: DecoderCtx](
        instance: Expr[JsonValueCodec[A]]
    ): MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Found cached codec instance for ${Type[A].prettyPrint}") >> MIO.pure(
        Rule.matched(Expr.quote {
          Expr.splice(instance).decodeValue(Expr.splice(dctx.reader), Expr.splice(instance).nullValue)
        })
      )

    private def callCachedHelper[A: DecoderCtx](
        helperCall: (Expr[JsonReader], Expr[JsoniterConfig]) => Expr[A]
    ): MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Found cached decoder helper for ${Type[A].prettyPrint}") >> MIO.pure(
        Rule.matched(helperCall(dctx.reader, dctx.config))
      )

    private def yieldUnsupported[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached decoder"))
  }

}
