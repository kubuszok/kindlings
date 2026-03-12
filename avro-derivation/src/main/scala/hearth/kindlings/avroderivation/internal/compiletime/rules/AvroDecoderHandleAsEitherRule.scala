package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils
import org.apache.avro.Schema

trait AvroDecoderHandleAsEitherRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  @scala.annotation.nowarn("msg=is never used")
  object AvroDecoderHandleAsEitherRule extends DecoderDerivationRule("handle as Either when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as Either") >> {
        Type[A] match {
          case IsEither(isEither) =>
            import isEither.{LeftValue, RightValue}
            implicit val AnyT: Type[Any] = DecTypes.Any
            implicit val SchemaT: Type[Schema] = DecTypes.Schema

            for {
              leftDecoderBuilder <- LambdaBuilder
                .of1[Any]("leftRaw")
                .traverse { leftRawExpr =>
                  deriveDecoderRecursively[LeftValue](using dctx.nest[LeftValue](leftRawExpr))
                }
              rightDecoderBuilder <- LambdaBuilder
                .of1[Any]("rightRaw")
                .traverse { rightRawExpr =>
                  deriveDecoderRecursively[RightValue](using dctx.nest[RightValue](rightRawExpr))
                }
              unionSchemaExpr <- deriveSelfContainedSchema[A](dctx.config)
            } yield {
              val leftDecodeFn = leftDecoderBuilder.build[LeftValue]
              val rightDecodeFn = rightDecoderBuilder.build[RightValue]
              Rule.matched(Expr.quote {
                AvroDerivationUtils
                  .decodeEither(
                    Expr.splice(dctx.avroValue),
                    Expr.splice(unionSchemaExpr),
                    Expr.splice(leftDecodeFn),
                    Expr.splice(rightDecodeFn)
                  )
                  .asInstanceOf[A]
              })
            }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Either"))
        }
      }
  }
}
