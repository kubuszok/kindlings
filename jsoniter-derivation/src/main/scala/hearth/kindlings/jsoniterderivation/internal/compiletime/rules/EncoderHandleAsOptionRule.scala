package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

trait EncoderHandleAsOptionRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsOptionRule extends EncoderDerivationRule("handle as Option when possible") {
    implicit val UnitT: Type[Unit] = CTypes.Unit

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as Option") >> {
        Type[A] match {
          case IsOption(isOption) =>
            import isOption.Underlying as Inner
            LambdaBuilder
              .of1[Inner]("inner")
              .traverse { innerExpr =>
                deriveEncoderRecursively[Inner](using ectx.nest(innerExpr))
              }
              .map { builder =>
                val lambda = builder.build[Unit]
                Rule.matched(
                  isOption.value.fold[Unit](ectx.value)(
                    onEmpty = Expr.quote(Expr.splice(ectx.writer).writeNull()),
                    onSome = innerExpr =>
                      Expr.quote {
                        Expr.splice(lambda).apply(Expr.splice(innerExpr))
                      }
                  )
                )
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Option"))
        }
      }
  }
}
