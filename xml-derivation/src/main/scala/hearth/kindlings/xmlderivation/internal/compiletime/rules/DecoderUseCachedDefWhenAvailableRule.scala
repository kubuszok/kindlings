package hearth.kindlings.xmlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.xmlderivation.{XmlConfig, XmlDecodingError}

trait DecoderUseCachedDefWhenAvailableRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderUseCachedDefWhenAvailableRule extends DecoderDerivationRule("use cached def when available") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      Log.info(s"Attempting to use cached XML decoder for ${Type[A].prettyPrint}") >>
        dctx.getInstance[A].flatMap {
          case Some(instance) => callCachedInstance[A](instance)
          case None           =>
            dctx.getHelper[A].flatMap {
              case Some(helperCall) => callCachedHelper[A](helperCall)
              case None             => yieldUnsupported[A]
            }
        }

    @scala.annotation.nowarn("msg=is never used")
    private def callCachedInstance[A: DecoderCtx](
        instance: Expr[hearth.kindlings.xmlderivation.XmlDecoder[A]]
    ): MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] = {
      implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
      implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
      implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
      implicit val DecoderAT: Type[hearth.kindlings.xmlderivation.XmlDecoder[A]] = DTypes.XmlDecoder[A]
      val publicInstance: Expr[hearth.kindlings.xmlderivation.XmlDecoder[A]] =
        instance.upcast[hearth.kindlings.xmlderivation.XmlDecoder[A]]
      Log.info(s"Found cached XML decoder instance for ${Type[A].prettyPrint}") >> MIO.pure(Rule.matched(Expr.quote {
        Expr.splice(publicInstance).decode(Expr.splice(dctx.elem))
      }))
    }

    private def callCachedHelper[A: DecoderCtx](
        helperCall: (Expr[scala.xml.Elem], Expr[XmlConfig]) => Expr[Either[XmlDecodingError, A]]
    ): MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      Log.info(s"Found cached XML decoder helper for ${Type[A].prettyPrint}") >> MIO.pure(
        Rule.matched(helperCall(dctx.elem, dctx.config))
      )

    private def yieldUnsupported[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached XML decoder"))
  }

}
