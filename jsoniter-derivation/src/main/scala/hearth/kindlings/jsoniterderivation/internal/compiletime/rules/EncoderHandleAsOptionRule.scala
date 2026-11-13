package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils

trait EncoderHandleAsOptionRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsOptionRule extends EncoderDerivationRule("handle as Option when possible") {
    implicit val UnitT: Type[Unit] = CTypes.Unit

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as Option") >> {
        Type[A] match {
          case _ if nullableUnionUnderlying[A].isDefined =>
            // `T | Null` (not via the helper cache: `String | Null` =:= `String` would find this helper): `null` or the `T`
            val underlying = nullableUnionUnderlying[A].get
            import underlying.Underlying as U
            val valueAsU: Expr[U] = Expr.quote(Expr.splice(ectx.value).asInstanceOf[U])
            deriveEncoderViaRules[U](using ectx.nest(valueAsU)).map { encodeU =>
              Rule.matched(Expr.quote {
                if (Expr.splice(ectx.value).asInstanceOf[AnyRef] eq null) Expr.splice(ectx.writer).writeNull()
                else Expr.splice(encodeU)
              })
            }
          case IsOption(isOption) =>
            import isOption.Underlying as Inner
            val innerIsOption = Inner match {
              case IsOption(_) => true
              case _           => false
            }
            LambdaBuilder
              .of1[Inner]("inner")
              .traverse { innerExpr =>
                if (innerIsOption) encodeNestedOption[Inner](using ectx.nest(innerExpr))
                else deriveEncoderRecursively[Inner](using ectx.nest(innerExpr))
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

    /** An `Option` nested in an `Option`: with `skipNestedOptionValues` encoded as any other `Option` (`null` / the
      * value), otherwise as jsoniter-scala's ADT `Option` (see `JsoniterDerivationUtils.writeNestedOptionAsAdt`), whose
      * `value` is again in a nested position when it is an `Option`.
      */
    @scala.annotation.nowarn("msg=is never used")
    private def encodeNestedOption[O: EncoderCtx]: MIO[Expr[Unit]] = {
      val skipNested: Option[Boolean] = ectx.evaluatedConfig.map(_.skipNestedOptionValues)
      val asPlainOption: MIO[Option[Expr[Unit]]] =
        if (skipNested.contains(false)) MIO.pure(None) else deriveEncoderRecursively[O].map(Some(_))
      val asAdt: MIO[Option[Expr[Unit]]] =
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
                .of1[Value]("value")
                .traverse { valueExpr =>
                  if (valueIsOption) encodeNestedOption[Value](using ectx.nest(valueExpr))
                  else deriveEncoderRecursively[Value](using ectx.nest(valueExpr))
                }
                .map { builder =>
                  val lambda = builder.build[Unit]
                  Some(Expr.quote {
                    JsoniterDerivationUtils.writeNestedOptionAsAdt[Value](
                      Expr.splice(ectx.writer),
                      Expr.splice(ectx.value).asInstanceOf[Option[Value]],
                      Expr.splice(ectx.config)
                    )(Expr.splice(lambda))
                  })
                }
            case _ => MIO.pure(None)
          }
      asPlainOption.flatMap(p => asAdt.map(a => (p, a))).map {
        case (Some(plain), Some(adt)) =>
          Expr.quote {
            if (Expr.splice(ectx.config).skipNestedOptionValues) Expr.splice(plain) else Expr.splice(adt)
          }
        case (Some(plain), None) => plain
        case (None, Some(adt))   => adt
        case (None, None)        => Expr.quote(())
      }
    }
  }
}
