package hearth.kindlings.xmlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.xmlderivation.{KindlingsXmlDecoder, XmlDecodingError}

trait DecoderUseImplicitWhenAvailableRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderUseImplicitWhenAvailableRule extends DecoderDerivationRule("use implicit when available") {

    lazy val ignoredImplicits: Seq[UntypedMethod] = {
      val ours = Type.of[KindlingsXmlDecoder.type].methods.collect {
        case method if method.value.name == "derived" => method.value.asUntyped
      }
      ours
    }

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      Log.info(s"Attempting to use implicit XmlDecoder for ${Type[A].prettyPrint}") >> {
        if (dctx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          DTypes.XmlDecoder[A].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(instanceExpr) => cacheAndUse[A](instanceExpr)
            case Left(reason)        => yieldUnsupported[A](reason)
          }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def cacheAndUse[A: DecoderCtx](
        instanceExpr: Expr[hearth.kindlings.xmlderivation.XmlDecoder[A]]
    ): MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] = {
      implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
      implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
      implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
      implicit val DecoderAT: Type[hearth.kindlings.xmlderivation.XmlDecoder[A]] = DTypes.XmlDecoder[A]
      val publicInstance: Expr[hearth.kindlings.xmlderivation.XmlDecoder[A]] =
        instanceExpr.upcast[hearth.kindlings.xmlderivation.XmlDecoder[A]]
      Log.info(s"Found implicit XML decoder ${instanceExpr.prettyPrint}, using directly") >>
        MIO.pure(Rule.matched(Expr.quote {
          Expr.splice(publicInstance).decode(Expr.splice(dctx.elem))
        }))
    }

    private def yieldUnsupported[A: DecoderCtx](
        reason: String
    ): MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      MIO.pure(
        Rule.yielded(
          s"The type ${Type[A].prettyPrint} does not have an implicit XmlDecoder instance: $reason"
        )
      )
  }

}
