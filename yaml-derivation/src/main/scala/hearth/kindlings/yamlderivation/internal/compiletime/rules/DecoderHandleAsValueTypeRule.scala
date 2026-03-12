package hearth.kindlings.yamlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import org.virtuslab.yaml.ConstructError

trait DecoderHandleAsValueTypeRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsValueTypeRule extends DecoderDerivationRule("handle as value type when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a value type") >> {
        Type[A] match {
          case IsValueType(isValueType) =>
            import isValueType.Underlying as Inner

            DTypes
              .YamlDecoder[Inner]
              .summonExprIgnoring(DecoderUseImplicitWhenAvailableRule.ignoredImplicits*)
              .toEither match {
              case Right(innerDecoder) =>
                isValueType.value.wrap match {
                  case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
                    @scala.annotation.nowarn("msg=is never used")
                    implicit val EitherCEA: Type[Either[ConstructError, A]] = DTypes.DecoderResult[A]
                    LambdaBuilder
                      .of1[Inner]("inner")
                      .traverse { innerExpr =>
                        val wrapResult = isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[Either[String, A]]]
                        MIO.pure(Expr.quote {
                          Expr.splice(wrapResult).left.map { (msg: String) =>
                            ConstructError.from(msg, Expr.splice(dctx.node))
                          }
                        })
                      }
                      .map { builder =>
                        val wrapLambda = builder.build[Either[ConstructError, A]]
                        Rule.matched(Expr.quote {
                          Expr
                            .splice(innerDecoder)
                            .construct(Expr.splice(dctx.node))(org.virtuslab.yaml.LoadSettings.empty)
                            .flatMap(Expr.splice(wrapLambda))
                        })
                      }
                  case _ =>
                    LambdaBuilder
                      .of1[Inner]("inner")
                      .traverse { innerExpr =>
                        MIO.pure(isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[A]])
                      }
                      .map { builder =>
                        val wrapLambda = builder.build[A]
                        Rule.matched(Expr.quote {
                          Expr
                            .splice(innerDecoder)
                            .construct(Expr.splice(dctx.node))(org.virtuslab.yaml.LoadSettings.empty)
                            .map(Expr.splice(wrapLambda))
                        })
                      }
                }
              case Left(reason) =>
                MIO.pure(Rule.yielded(s"Value type inner ${Type[Inner].prettyPrint} has no YamlDecoder: $reason"))
            }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
        }
      }
  }
}
