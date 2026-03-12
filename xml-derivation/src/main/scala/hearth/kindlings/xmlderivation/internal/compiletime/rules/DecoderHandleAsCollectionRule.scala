package hearth.kindlings.xmlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.xmlderivation.XmlDecodingError

trait DecoderHandleAsCollectionRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsCollectionRule extends DecoderDerivationRule("handle as collection when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
        implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
        implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            implicit val EitherItemT: Type[Either[XmlDecodingError, Item]] = DTypes.DecoderResult[Item]
            LambdaBuilder
              .of1[scala.xml.Elem]("collItemElem")
              .traverse { itemElemExpr =>
                deriveDecoderRecursively[Item](using dctx.nest[Item](itemElemExpr))
              }
              .map { builder =>
                val lambda = builder.build[Either[XmlDecodingError, Item]]
                Rule.matched(Expr.quote {
                  val parentElem = Expr.splice(dctx.elem)
                  val childElems = parentElem.child.collect { case e: scala.xml.Elem => e }.toList
                  val results: List[Either[XmlDecodingError, Item]] = childElems.map { childElem =>
                    Expr.splice(lambda).apply(childElem)
                  }
                  val errors = results.collect { case Left(e) => e }
                  if (errors.nonEmpty) Left(XmlDecodingError.Multiple(errors).asInstanceOf[XmlDecodingError])
                  else Right(results.collect { case Right(v) => v }.asInstanceOf[A])
                })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection"))
        }
      }
  }

}
