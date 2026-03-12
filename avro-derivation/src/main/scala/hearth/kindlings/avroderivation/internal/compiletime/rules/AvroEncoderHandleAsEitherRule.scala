package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

trait AvroEncoderHandleAsEitherRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  object AvroEncoderHandleAsEitherRule extends EncoderDerivationRule("handle as Either when possible") {
    implicit val AnyT: Type[Any] = EncTypes.Any

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Any]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as Either") >> {
        Type[A] match {
          case IsEither(isEither) =>
            import isEither.{LeftValue, RightValue}
            for {
              leftBuilder <- LambdaBuilder
                .of1[LeftValue]("leftVal")
                .traverse { leftExpr =>
                  deriveEncoderRecursively[LeftValue](using ectx.nest(leftExpr))
                }
              rightBuilder <- LambdaBuilder
                .of1[RightValue]("rightVal")
                .traverse { rightExpr =>
                  deriveEncoderRecursively[RightValue](using ectx.nest(rightExpr))
                }
            } yield {
              val leftLambda = leftBuilder.build[Any]
              val rightLambda = rightBuilder.build[Any]
              Rule.matched(
                isEither.value.fold[Any](ectx.value)(
                  onLeft = leftExpr =>
                    Expr.quote {
                      Expr.splice(leftLambda).apply(Expr.splice(leftExpr))
                    },
                  onRight = rightExpr =>
                    Expr.quote {
                      Expr.splice(rightLambda).apply(Expr.splice(rightExpr))
                    }
                )
              )
            }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Either"))
        }
      }
  }
}
