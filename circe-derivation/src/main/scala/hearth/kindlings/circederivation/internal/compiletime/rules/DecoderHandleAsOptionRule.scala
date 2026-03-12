package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils
import io.circe.{DecodingFailure, HCursor}

trait DecoderHandleAsOptionRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsOptionRule extends DecoderDerivationRule("handle as Option when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as Option") >> {
        Type[A] match {
          case IsOption(isOption) =>
            import isOption.Underlying as Inner
            implicit val HCursorT: Type[HCursor] = DTypes.HCursor
            implicit val EitherDFInner: Type[Either[DecodingFailure, Inner]] = DTypes.DecoderResult[Inner]

            LambdaBuilder
              .of1[HCursor]("innerCursor")
              .traverse { innerCursorExpr =>
                deriveDecoderRecursively[Inner](using dctx.nest[Inner](innerCursorExpr))
              }
              .map { builder =>
                val decodeFn = builder.build[Either[DecodingFailure, Inner]]
                Rule.matched(Expr.quote {
                  CirceDerivationUtils
                    .decodeOptionFromFn(
                      Expr.splice(dctx.cursor),
                      Expr.splice(decodeFn)
                    )
                    .asInstanceOf[Either[DecodingFailure, A]]
                })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Option"))
        }
      }
  }
}
