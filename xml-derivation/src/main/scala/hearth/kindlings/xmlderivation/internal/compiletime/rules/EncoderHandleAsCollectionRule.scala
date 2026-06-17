package hearth.kindlings.xmlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationUtils

trait EncoderHandleAsCollectionRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsCollectionRule extends EncoderDerivationRule("handle as collection or map when possible") {
    implicit val ElemT: Type[scala.xml.Elem] = Types.Elem
    implicit val StringT: Type[String] = Types.String

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[scala.xml.Elem]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection or map") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            // A map is an `IsCollectionOf` whose proof is an `IsMapOf`. Parse `IsCollection` ONCE and dispatch on
            // that, instead of the old map-rule (`IsMap.parse` = `IsCollection.parse` + cast) followed by a separate
            // collection rule that parsed `IsCollection` again — every non-map field paid for the parse twice.
            import isCollection.Underlying as Item
            isCollection.value.asMap match {
              case Some(isMapOf) =>
                EncoderHandleAsMapRule.deriveMapEntries[A, Item](isMapOf)
              case None =>
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
            }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection or map"))
        }
      }
  }

}
