package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait AvroEncoderUseCachedDefWhenAvailableRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  object AvroEncoderUseCachedDefWhenAvailableRule extends EncoderDerivationRule("use cached def when available") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Any]]] =
      Log.info(s"Attempting to use cached encoder for ${Type[A].prettyPrint}") >>
        ectx.getInstance[A].flatMap {
          case Some(instance) =>
            Log.info(s"Found cached encoder instance for ${Type[A].prettyPrint}") >> MIO.pure(
              Rule.matched(Expr.quote {
                Expr.splice(instance).encode(Expr.splice(ectx.value))
              })
            )
          case None =>
            ectx.getHelper[A].flatMap {
              case Some(helperCall) =>
                Log.info(s"Found cached encoder helper for ${Type[A].prettyPrint}") >> MIO.pure(
                  Rule.matched(helperCall(ectx.value, ectx.config))
                )
              case None =>
                MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached encoder"))
            }
        }
  }
}
