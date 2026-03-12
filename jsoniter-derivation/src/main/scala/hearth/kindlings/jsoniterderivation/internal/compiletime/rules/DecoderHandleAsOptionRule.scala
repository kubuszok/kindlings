package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils
import com.github.plokhotnyuk.jsoniter_scala.core.JsonReader

trait DecoderHandleAsOptionRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsOptionRule extends DecoderDerivationRule("handle as Option when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as Option") >> {
        Type[A] match {
          case IsOption(isOption) =>
            import isOption.Underlying as Inner
            implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader

            LambdaBuilder
              .of1[JsonReader]("innerReader")
              .traverse { innerReaderExpr =>
                deriveDecoderRecursively[Inner](using dctx.nest[Inner](innerReaderExpr))
              }
              .map { builder =>
                val decodeFn = builder.build[Inner]
                Rule.matched(Expr.quote {
                  JsoniterDerivationUtils
                    .readOption(Expr.splice(dctx.reader))(Expr.splice(decodeFn))
                    .asInstanceOf[A]
                })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Option"))
        }
      }
  }
}
