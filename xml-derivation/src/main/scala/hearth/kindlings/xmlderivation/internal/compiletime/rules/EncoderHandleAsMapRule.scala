package hearth.kindlings.xmlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationUtils

trait EncoderHandleAsMapRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsMapRule extends EncoderDerivationRule("handle as map when possible") {
    implicit val ElemT: Type[scala.xml.Elem] = Types.Elem
    implicit val StringT: Type[String] = Types.String

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[scala.xml.Elem]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a map") >> {
        Type[A] match {
          case IsMap(isMap) =>
            import isMap.Underlying as Pair
            deriveMapEntries[A, Pair](isMap.value)

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a map"))
        }
      }

    private def deriveMapEntries[A: EncoderCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[scala.xml.Elem]]] = {
      import isMap.{Key, Value}
      if (!(Key <:< Type[String]))
        MIO.pure(Rule.yielded(s"Map key type ${Key.prettyPrint} is not String"))
      else {
        LambdaBuilder
          .of2[Value, String]("mapValue", "itemElementName")
          .traverse { case (valueExpr, itemNameExpr) =>
            deriveEncoderRecursively[Value](using ectx.nest(valueExpr).copy(elementName = itemNameExpr))
          }
          .map { builder =>
            val lambda = builder.build[scala.xml.Elem]
            val iterableExpr = isMap.asIterable(ectx.value)
            Rule.matched(Expr.quote {
              val entries = Expr.splice(iterableExpr).asInstanceOf[Iterable[(String, Value)]]
              val children = XmlDerivationUtils.encodeMappedPairs[Value](
                entries,
                "entry",
                "key",
                (v: Value, n: String) => Expr.splice(lambda).apply(v, n)
              )
              XmlDerivationUtils.makeElem(Expr.splice(ectx.elementName), Nil, children)
            })
          }
      }
    }
  }

}
