package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait AvroDecoderHandleAsValueTypeRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  object AvroDecoderHandleAsValueTypeRule extends DecoderDerivationRule("handle as value type when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a value type") >> {
        Type[A] match {
          case IsValueType(isValueType) =>
            import isValueType.Underlying as Inner
            for {
              innerResult <- deriveDecoderRecursively[Inner](using dctx.nest[Inner](dctx.avroValue))
            } yield isValueType.value.wrap match {
              case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
                // Wrap returns Either[String, A] — throw AvroRuntimeException on Left
                val eitherResult = isValueType.value.wrap.apply(innerResult).asInstanceOf[Expr[Either[String, A]]]
                Rule.matched(Expr.quote {
                  Expr.splice(eitherResult) match {
                    case scala.Right(v)  => v
                    case scala.Left(msg) => throw new org.apache.avro.AvroRuntimeException(msg)
                  }
                })
              case _ =>
                // PlainValue — original behavior
                Rule.matched(isValueType.value.wrap.apply(innerResult).asInstanceOf[Expr[A]])
            }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
        }
      }
  }
}
