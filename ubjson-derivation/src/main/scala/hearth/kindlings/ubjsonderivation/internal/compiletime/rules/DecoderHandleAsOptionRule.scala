package hearth.kindlings.ubjsonderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.ubjsonderivation.UBJsonReader
import hearth.kindlings.ubjsonderivation.internal.runtime.UBJsonDerivationUtils

trait DecoderHandleAsOptionRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsOptionRule extends DecoderDerivationRule("handle as Option when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as Option") >> {
        Type[A] match {
          case _ if nullableUnionUnderlying[A].isDefined =>
            // `T | Null` (Scala 3): `null` or a `T` (as jsoniter-derivation)
            val underlying = nullableUnionUnderlying[A].get
            import underlying.Underlying as U
            deriveDecoderViaRules[U](using dctx.nest[U](dctx.reader)).map { decodeU =>
              Rule.matched(Expr.quote {
                if (Expr.splice(dctx.reader).isNextNull()) null.asInstanceOf[A]
                else Expr.splice(decodeU).asInstanceOf[A]
              })
            }
          case IsOption(isOption) =>
            import isOption.Underlying as Inner
            implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader
            val isNested = Inner match {
              case IsOption(_) => true
              case _           => false
            }

            LambdaBuilder
              .of1[UBJsonReader]("innerReader")
              .traverse { innerReaderExpr =>
                if (isNested) decodeNestedOption[Inner](using dctx.nest[Inner](innerReaderExpr))
                else deriveDecoderRecursively[Inner](using dctx.nest[Inner](innerReaderExpr))
              }
              .map { builder =>
                val decodeFn = builder.build[Inner]
                if (isNested) {
                  // `skipNestedOptionValues`: `null` decodes as `Some(None)` (as jsoniter-scala)
                  implicit val BooleanT: Type[Boolean] = CTypes.Boolean
                  val skipNested: Expr[Boolean] = dctx.evaluatedConfig match {
                    case Some(c) => Expr(c.skipNestedOptionValues)
                    case None    => Expr.quote(Expr.splice(dctx.config).skipNestedOptionValues)
                  }
                  Rule.matched(Expr.quote {
                    UBJsonDerivationUtils
                      .readNestedOption(Expr.splice(dctx.reader), Expr.splice(skipNested))(Expr.splice(decodeFn))
                      .asInstanceOf[A]
                  })
                } else
                  Rule.matched(Expr.quote {
                    UBJsonDerivationUtils
                      .readOption(Expr.splice(dctx.reader))(Expr.splice(decodeFn))
                      .asInstanceOf[A]
                  })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Option"))
        }
      }

    /** Inverse of `EncoderHandleAsOptionRule.encodeNestedOption`. */
    @scala.annotation.nowarn("msg=is never used")
    private def decodeNestedOption[O: DecoderCtx]: MIO[Expr[O]] = {
      implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader
      val skipNested: Option[Boolean] = dctx.evaluatedConfig.map(_.skipNestedOptionValues)
      val asPlainOption: MIO[Option[Expr[O]]] =
        if (skipNested.contains(false)) MIO.pure(None) else deriveDecoderRecursively[O].map(Some(_))
      val asAdt: MIO[Option[Expr[O]]] =
        if (skipNested.contains(true)) MIO.pure(None)
        else
          Type[O] match {
            case IsOption(isOption) =>
              import isOption.Underlying as Value
              val valueIsOption = Value match {
                case IsOption(_) => true
                case _           => false
              }
              LambdaBuilder
                .of1[UBJsonReader]("valueReader")
                .traverse { valueReaderExpr =>
                  if (valueIsOption) decodeNestedOption[Value](using dctx.nest[Value](valueReaderExpr))
                  else deriveDecoderRecursively[Value](using dctx.nest[Value](valueReaderExpr))
                }
                .map { builder =>
                  val decodeFn = builder.build[Value]
                  Some(Expr.quote {
                    UBJsonDerivationUtils
                      .readNestedOptionAsAdt[Value](Expr.splice(dctx.reader), Expr.splice(dctx.config))(
                        Expr.splice(decodeFn)
                      )
                      .asInstanceOf[O]
                  })
                }
            case _ => MIO.pure(None)
          }
      asPlainOption.flatMap(p => asAdt.map(a => (p, a))).flatMap {
        case (Some(plain), Some(adt)) =>
          MIO.pure(Expr.quote {
            if (Expr.splice(dctx.config).skipNestedOptionValues) Expr.splice(plain) else Expr.splice(adt)
          })
        case (Some(plain), None) => MIO.pure(plain)
        case (None, Some(adt))   => MIO.pure(adt)
        case (None, None)        => MIO.fail(new IllegalStateException(s"${Type[O].prettyPrint} is not an Option"))
      }
    }
  }
}
