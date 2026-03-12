package hearth.kindlings.xmlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.xmlderivation.XmlDecodingError

trait DecoderHandleAsOptionRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsOptionRule extends DecoderDerivationRule("handle as Option when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as Option") >> {
        implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
        implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
        implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
        Type[A] match {
          case IsOption(isOption) =>
            import isOption.Underlying as Inner
            implicit val EitherInnerT: Type[Either[XmlDecodingError, Inner]] = DTypes.DecoderResult[Inner]
            // For Option, we try to decode and wrap in Some; if the element is effectively empty, return None
            LambdaBuilder
              .of1[scala.xml.Elem]("optElem")
              .traverse { elemExpr =>
                deriveDecoderRecursively[Inner](using dctx.nest[Inner](elemExpr))
              }
              .map { builder =>
                val lambda = builder.build[Either[XmlDecodingError, Inner]]
                Rule.matched(Expr.quote {
                  val elem = Expr.splice(dctx.elem)
                  if (elem.child.isEmpty && elem.attributes.isEmpty && elem.text.trim.isEmpty)
                    Right(None.asInstanceOf[A])
                  else
                    Expr.splice(lambda).apply(elem).map(v => Some(v).asInstanceOf[A])
                })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Option"))
        }
      }
  }

}
