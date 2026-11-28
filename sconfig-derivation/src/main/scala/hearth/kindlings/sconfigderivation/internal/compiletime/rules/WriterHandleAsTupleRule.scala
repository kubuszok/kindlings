package hearth.kindlings.sconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.instances.*
import hearth.fp.syntax.*
import hearth.std.*

import org.ekrich.config.ConfigValue
import hearth.kindlings.sconfigderivation.internal.runtime.SConfigDerivationUtils

trait WriterHandleAsTupleRuleImpl {
  this: WriterMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  /** Tuples are written the way upstream PureConfig's derivations write them: as the list of their elements, each
    * written with the writer of its type. On Scala 3 this includes the tuples which are not a `TupleN` (`EmptyTuple`,
    * `H *: T`, more than 22 elements), whose elements are read with `productElement`.
    */
  object WriterHandleAsTupleRule extends WriterDerivationRule("handle as tuple when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: WriterCtx]: MIO[Rule.Applicability[Expr[ConfigValue]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a tuple") >> {
        if (!Type[A].isTuple || Type[A].isNamedTuple)
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a tuple"))
        else
          genericTupleElementTypes[A] match {
            case Some(elementTypes) =>
              implicit val ProductT: Type[Product] = WTypes.Product
              val elements = elementTypes.zipWithIndex.map { case (elementType, index) =>
                import elementType.Underlying as Element
                Expr.quote {
                  Expr
                    .splice(wctx.value)
                    .asInstanceOf[Product]
                    .productElement(Expr.splice(Expr(index)))
                    .asInstanceOf[Element]
                }.as_??
              }
              encodeTuple[A](elements).map(Rule.matched)
            case None =>
              CaseClass.parse[A].toEither match {
                case Right(caseClass) =>
                  val elements = caseClass.caseFieldValuesAt(wctx.value).toList.map(_._2)
                  if (elements.isEmpty) MIO.pure(Rule.yielded(s"The tuple ${Type[A].prettyPrint} has no elements"))
                  else encodeTuple[A](elements).map(Rule.matched)
                case Left(reason) => MIO.pure(Rule.yielded(reason))
              }
          }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def encodeTuple[A: WriterCtx](elements: List[Expr_??]): MIO[Expr[ConfigValue]] = {
      implicit val ConfigValueT: Type[ConfigValue] = WTypes.ConfigValue

      elements.zipWithIndex
        .parTraverse { case (element, index) =>
          import element.{Underlying as Element, value as elementExpr}
          Log.namedScope(s"Writing tuple element ${index + 1}: ${Type[Element].prettyPrint}") {
            deriveWriterRecursively[Element](using wctx.nest(elementExpr))
          }
        }
        .map { written =>
          // One statement per element (no element is written inside an argument list).
          val addElements: Expr[java.util.ArrayList[ConfigValue]] => Expr[Unit] = { list =>
            written.foldLeft(Expr.quote(()): Expr[Unit]) { (acc, value) =>
              Expr.quote {
                Expr.splice(acc)
                SConfigDerivationUtils.addElement(Expr.splice(list), Expr.splice(value))
              }
            }
          }
          Expr.quote {
            val tupleElements = new java.util.ArrayList[ConfigValue](Expr.splice(Expr(written.size)))
            Expr.splice(addElements(Expr.quote(tupleElements)))
            SConfigDerivationUtils.elementsToList(tupleElements)
          }
        }
    }
  }
}
