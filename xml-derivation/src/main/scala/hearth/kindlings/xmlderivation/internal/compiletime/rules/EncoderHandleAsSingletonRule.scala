package hearth.kindlings.xmlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationUtils

trait EncoderHandleAsSingletonRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsSingletonRule extends EncoderDerivationRule("handle as singleton when possible") {
    implicit val ElemT: Type[scala.xml.Elem] = Types.Elem

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[scala.xml.Elem]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a singleton") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) if caseClass.primaryConstructor.parameters.flatten.isEmpty =>
            MIO.pure(Rule.matched(Expr.quote {
              XmlDerivationUtils.makeEmptyElem(Expr.splice(ectx.elementName))
            }))
          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a singleton/empty case class"))
        }
      }
  }

}
