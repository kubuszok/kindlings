package hearth.kindlings.xmlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationUtils

trait EncoderHandleAsCollectionRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsCollectionRule extends EncoderDerivationRule("handle as collection when possible") {
    implicit val ElemT: Type[scala.xml.Elem] = Types.Elem
    implicit val StringT: Type[String] = Types.String

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[scala.xml.Elem]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            LambdaBuilder
              .of2[Item, String]("collItem", "itemElementName")
              .traverse { case (itemExpr, itemNameExpr) =>
                deriveEncoderRecursively[Item](using ectx.nest(itemExpr).copy(elementName = itemNameExpr))
              }
              .map { builder =>
                val lambda = builder.build[scala.xml.Elem]
                val iterableExpr = isCollection.value.asIterable(ectx.value)
                Rule.matched(Expr.quote {
                  val items = Expr.splice(iterableExpr)
                  val children = XmlDerivationUtils.encodeIterable[Item](
                    items,
                    "item",
                    (i: Item, n: String) => Expr.splice(lambda).apply(i, n)
                  )
                  XmlDerivationUtils.makeElem(Expr.splice(ectx.elementName), Nil, children)
                })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection"))
        }
      }
  }

}
