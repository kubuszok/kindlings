package hearth.kindlings.xmlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.xmlderivation.XmlDecodingError
import hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationUtils

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
                // An empty element is how `None` is written, unless `withTransientNone` (no element for `None`): then
                // it is `Some` of the decoded empty value (`Some("")`), or `None` when it does not decode.
                def decodeEmpty(elem: Expr[scala.xml.Elem]): Expr[Either[XmlDecodingError, A]] = {
                  val none: Expr[Either[XmlDecodingError, A]] = Expr.quote(Right(None.asInstanceOf[A]))
                  val decodedOrNone: Expr[Either[XmlDecodingError, A]] = Expr.quote {
                    XmlDerivationUtils
                      .decodedOrNone(Expr.splice(lambda).apply(Expr.splice(elem)))
                      .asInstanceOf[Either[XmlDecodingError, A]]
                  }
                  dctx.evaluatedConfig.map(_.transientNone) match {
                    case Some(true)  => decodedOrNone
                    case Some(false) => none
                    case None        =>
                      Expr.quote {
                        if (Expr.splice(dctx.config).transientNone) Expr.splice(decodedOrNone) else Expr.splice(none)
                      }
                  }
                }
                Rule.matched(
                  Expr.quote {
                    val elem = Expr.splice(dctx.elem)
                    if (XmlDerivationUtils.isEmptyElement(elem)) Expr.splice(decodeEmpty(Expr.quote(elem)))
                    else Expr.splice(lambda).apply(elem).map(v => Some(v).asInstanceOf[A])
                  }
                )
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Option"))
        }
      }
  }

}
