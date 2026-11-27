package hearth.kindlings.sconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import org.ekrich.config.ConfigValue
import hearth.kindlings.sconfigderivation.internal.runtime.SConfigDerivationUtils

trait WriterHandleAsTupleRuleImpl {
  this: WriterMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  /** Tuples (`TupleN`) are written the way upstream PureConfig's derivations write them: as the list of their elements,
    * each written with the writer of its type.
    */
  object WriterHandleAsTupleRule extends WriterDerivationRule("handle as tuple when possible") {

    def apply[A: WriterCtx]: MIO[Rule.Applicability[Expr[ConfigValue]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a tuple") >> {
        if (!Type[A].isTuple || Type[A].isNamedTuple)
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a tuple"))
        else
          CaseClass.parse[A].toEither match {
            case Right(caseClass) =>
              NonEmptyList.fromList(caseClass.caseFieldValuesAt(wctx.value).toList) match {
                case Some(elements) => encodeTuple[A](elements).map(Rule.matched)
                case None           => MIO.pure(Rule.yielded(s"The tuple ${Type[A].prettyPrint} has no elements"))
              }
            case Left(reason) => MIO.pure(Rule.yielded(reason))
          }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def encodeTuple[A: WriterCtx](elements: NonEmptyList[(String, Expr_??)]): MIO[Expr[ConfigValue]] = {
      implicit val ConfigValueT: Type[ConfigValue] = WTypes.ConfigValue

      elements
        .parTraverse { case (name, element) =>
          import element.{Underlying as Element, value as elementExpr}
          Log.namedScope(s"Writing tuple element $name: ${Type[Element].prettyPrint}") {
            deriveWriterRecursively[Element](using wctx.nest(elementExpr))
          }
        }
        .map { written =>
          // One statement per element (no element is written inside an argument list).
          val addElements: Expr[java.util.ArrayList[ConfigValue]] => Expr[Unit] = { list =>
            written.toList.foldLeft(Expr.quote(()): Expr[Unit]) { (acc, value) =>
              Expr.quote {
                Expr.splice(acc)
                SConfigDerivationUtils.addElement(Expr.splice(list), Expr.splice(value))
              }
            }
          }
          Expr.quote {
            val tupleElements = new java.util.ArrayList[ConfigValue](Expr.splice(Expr(written.toList.size)))
            Expr.splice(addElements(Expr.quote(tupleElements)))
            SConfigDerivationUtils.elementsToList(tupleElements)
          }
        }
    }
  }
}
