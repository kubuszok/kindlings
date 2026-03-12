package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait AvroDecoderUseCachedDefWhenAvailableRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  object AvroDecoderUseCachedDefWhenAvailableRule extends DecoderDerivationRule("use cached def when available") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to use cached decoder for ${Type[A].prettyPrint}") >>
        dctx.getInstance[A].flatMap {
          case Some(instance) =>
            Log.info(s"Found cached decoder instance for ${Type[A].prettyPrint}") >> MIO.pure(
              Rule.matched(Expr.quote {
                Expr.splice(instance).decode(Expr.splice(dctx.avroValue))
              })
            )
          case None =>
            dctx.getHelper[A].flatMap {
              case Some(helperCall) =>
                Log.info(s"Found cached decoder helper for ${Type[A].prettyPrint}") >> MIO.pure(
                  Rule.matched(helperCall(dctx.avroValue, dctx.config))
                )
              case None =>
                MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached decoder"))
            }
        }
  }
}
