package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils

trait AvroEncoderHandleAsValueTypeRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  object AvroEncoderHandleAsValueTypeRule extends EncoderDerivationRule("handle as value type when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Any]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a value type") >> {
        if (Type[A].isNamedTuple)
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is a named tuple, not a value type"))
        else
          Type[A] match {
            case IsValueType(isValueType) =>
              import isValueType.Underlying as Inner
              val unwrappedExpr = isValueType.value.unwrap(ectx.value)
              fixedValueTypeSize[A, Inner] match {
                // a value class annotated with @avroFixed: a FIXED value of the class's own (named) schema
                case Some(_) =>
                  implicit val AnyT: Type[Any] = EncTypes.Any
                  cachedEncodeSchema[A].map { schemaExpr =>
                    Rule.matched(
                      if (Type[Inner] =:= Type.of[String])
                        Expr.quote {
                          AvroDerivationUtils.wrapStringAsFixed(
                            Expr.splice(unwrappedExpr).asInstanceOf[String],
                            Expr.splice(schemaExpr)
                          ): Any
                        }
                      else
                        Expr.quote {
                          AvroDerivationUtils.wrapByteArrayAsFixed(
                            Expr.splice(unwrappedExpr).asInstanceOf[Array[Byte]],
                            Expr.splice(schemaExpr)
                          ): Any
                        }
                    )
                  }
                case None =>
                  for {
                    innerResult <- deriveEncoderRecursively[Inner](using ectx.nest(unwrappedExpr))
                  } yield Rule.matched(innerResult)
              }

            case _ =>
              MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
          }
      }
  }
}
