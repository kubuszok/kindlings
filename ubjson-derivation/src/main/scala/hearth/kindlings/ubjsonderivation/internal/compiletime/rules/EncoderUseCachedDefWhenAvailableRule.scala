package hearth.kindlings.ubjsonderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.ubjsonderivation.{UBJsonConfig, UBJsonValueCodec, UBJsonWriter}

trait EncoderUseCachedDefWhenAvailableRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderUseCachedDefWhenAvailableRule extends EncoderDerivationRule("use cached def when available") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to use cached encoder for ${Type[A].prettyPrint}") >>
        ectx.getInstance[A].flatMap {
          case Some(instance) =>
            callCachedInstance[A](instance)
          case None =>
            ectx.getHelper[A].flatMap {
              case Some(helperCall) =>
                callCachedHelper[A](helperCall)
              case None =>
                yieldUnsupported[A]
            }
        }

    @scala.annotation.nowarn("msg=is never used")
    private def callCachedInstance[A: EncoderCtx](
        instance: Expr[UBJsonValueCodec[A]]
    ): MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Found cached codec instance for ${Type[A].prettyPrint}") >> MIO.pure(Rule.matched(Expr.quote {
        Expr.splice(instance).encode(Expr.splice(ectx.writer), Expr.splice(ectx.value))
      }))

    private def callCachedHelper[A: EncoderCtx](
        helperCall: (Expr[A], Expr[UBJsonWriter], Expr[UBJsonConfig]) => Expr[Unit]
    ): MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Found cached encoder helper for ${Type[A].prettyPrint}") >> MIO.pure(
        Rule.matched(helperCall(ectx.value, ectx.writer, ectx.config))
      )

    private def yieldUnsupported[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached encoder"))
  }

}
