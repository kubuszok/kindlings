package hearth.kindlings.ubjsonderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.ubjsonderivation.UBJsonReader

trait DecoderHandleAsValueTypeRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsValueTypeRule extends DecoderDerivationRule("handle as value type when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a value type") >> {
        Type[A] match {
          case IsValueType(isValueType) =>
            import isValueType.Underlying as Inner
            val wrapLambda: Expr[Inner => A] = buildWrap[A, Inner](isValueType.value, dctx.reader)
            deriveDecoderRecursively[Inner](using dctx.nest[Inner](dctx.reader)).map { innerDecoded =>
              Rule.matched(Expr.quote {
                Expr.splice(wrapLambda).apply(Expr.splice(innerDecoded))
              })
            }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
        }
      }

    private def buildWrap[A: Type, Inner: Type](
        isValueType: IsValueTypeOf[A, Inner],
        readerExpr: Expr[UBJsonReader]
    ): Expr[Inner => A] = {
      @scala.annotation.nowarn("msg=is never used")
      implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader
      isValueType.wrap match {
        case _: CtorLikeOf.PlainValue[?, ?] =>
          Expr.quote { (inner: Inner) =>
            Expr.splice(isValueType.wrap.apply(Expr.quote(inner)).asInstanceOf[Expr[A]])
          }
        case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
          Expr.quote { (inner: Inner) =>
            Expr.splice(isValueType.wrap.apply(Expr.quote(inner)).asInstanceOf[Expr[Either[String, A]]]) match {
              case scala.Right(v)  => v
              case scala.Left(msg) => Expr.splice(readerExpr).decodeError(msg)
            }
          }
        case _: CtorLikeOf.EitherIterableStringOrValue[?, ?] =>
          Expr.quote { (inner: Inner) =>
            Expr
              .splice(isValueType.wrap.apply(Expr.quote(inner)).asInstanceOf[Expr[Either[Iterable[String], A]]]) match {
              case scala.Right(v)   => v
              case scala.Left(errs) => Expr.splice(readerExpr).decodeError(errs.mkString("\n"))
            }
          }
        case _: CtorLikeOf.EitherThrowableOrValue[?, ?] =>
          Expr.quote { (inner: Inner) =>
            Expr.splice(isValueType.wrap.apply(Expr.quote(inner)).asInstanceOf[Expr[Either[Throwable, A]]]) match {
              case scala.Right(v)  => v
              case scala.Left(err) => Expr.splice(readerExpr).decodeError(err.getMessage)
            }
          }
        case _: CtorLikeOf.EitherIterableThrowableOrValue[?, ?] =>
          Expr.quote { (inner: Inner) =>
            Expr
              .splice(
                isValueType.wrap.apply(Expr.quote(inner)).asInstanceOf[Expr[Either[Iterable[Throwable], A]]]
              ) match {
              case scala.Right(v)   => v
              case scala.Left(errs) => Expr.splice(readerExpr).decodeError(errs.map(_.getMessage).mkString("\n"))
            }
          }
      }
    }
  }

}
