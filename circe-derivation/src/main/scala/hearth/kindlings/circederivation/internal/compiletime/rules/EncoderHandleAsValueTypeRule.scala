package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import io.circe.Json

trait EncoderHandleAsValueTypeRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsValueTypeRule extends EncoderDerivationRule("handle as value type when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a value type") >> {
        if (Type[A].isNamedTuple)
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is a named tuple, not a value type"))
        else
          Type[A] match {
            case IsValueType(isValueType) =>
              import isValueType.Underlying as Inner
              val unwrappedExpr = isValueType.value.unwrap(ectx.value)
              val unwrappedMIO = deriveEncoderRecursively[Inner](using ectx.nest(unwrappedExpr))
              anyValCaseClass[A] match {
                case None => unwrappedMIO.map(Rule.matched)
                // An `AnyVal` case class: unwrapped by default, encoded as a product (like upstream circe) with
                // `unwrapValueClasses = false`.
                case Some(caseClass) =>
                  def productMIO = EncoderHandleAsCaseClassRule.encodeCaseClassFields[A](caseClass)
                  ectx.evaluatedConfig.map(_.unwrapValueClasses) match {
                    case Some(true)  => unwrappedMIO.map(Rule.matched)
                    case Some(false) => productMIO.map(Rule.matched)
                    case None        =>
                      implicit val JsonT: Type[Json] = Types.Json
                      for {
                        unwrapped <- unwrappedMIO
                        product <- productMIO
                      } yield Rule.matched(Expr.quote {
                        if (Expr.splice(ectx.config).unwrapValueClasses) Expr.splice(unwrapped)
                        else Expr.splice(product)
                      })
                  }
              }

            case _ =>
              MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
          }
      }
  }
}
