package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils

trait AvroDecoderHandleAsOptionRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  object AvroDecoderHandleAsOptionRule extends DecoderDerivationRule("handle as Option when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as Option") >> {
        Type[A] match {
          case IsOption(isOption) =>
            import isOption.Underlying as Inner
            implicit val AnyT: Type[Any] = DecTypes.Any

            LambdaBuilder
              .of1[Any]("innerValue")
              .traverse { innerValueExpr =>
                deriveDecoderRecursively[Inner](using dctx.nest[Inner](innerValueExpr))
              }
              .map { builder =>
                val decodeFn = builder.build[Inner]
                Rule.matched(Expr.quote {
                  AvroDerivationUtils
                    .decodeOption(Expr.splice(dctx.avroValue), Expr.splice(decodeFn))
                    .asInstanceOf[A]
                })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Option"))
        }
      }
  }
}
