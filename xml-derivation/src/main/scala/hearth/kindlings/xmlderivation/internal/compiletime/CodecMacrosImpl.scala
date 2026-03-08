package hearth.kindlings.xmlderivation.internal.compiletime

import hearth.MacroCommons
import hearth.std.*

import hearth.kindlings.xmlderivation.{KindlingsXmlCodec, XmlConfig}
import hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationUtils

trait CodecMacrosImpl {
  this: MacroCommons & StdExtensions & AnnotationSupport & EncoderMacrosImpl & DecoderMacrosImpl =>

  @scala.annotation.nowarn("msg=is never used")
  def deriveCodecTypeClass[A: Type](configExpr: Expr[XmlConfig]): Expr[KindlingsXmlCodec[A]] = {
    implicit val KindlingsXmlCodecA: Type[KindlingsXmlCodec[A]] = CTypes.KindlingsXmlCodec[A]

    // Step 1: Derive XmlEncoder[A]
    val encoderExpr: Expr[hearth.kindlings.xmlderivation.XmlEncoder[A]] =
      deriveEncoderTypeClass[A](configExpr).asInstanceOf[Expr[hearth.kindlings.xmlderivation.XmlEncoder[A]]]
    // Step 2: Derive XmlDecoder[A]
    val decoderExpr: Expr[hearth.kindlings.xmlderivation.XmlDecoder[A]] =
      deriveDecoderTypeClass[A](configExpr).asInstanceOf[Expr[hearth.kindlings.xmlderivation.XmlDecoder[A]]]
    // Step 3: Combine at runtime
    Expr.quote {
      XmlDerivationUtils.xmlCodec[A](
        Expr.splice(encoderExpr),
        Expr.splice(decoderExpr)
      )
    }
  }

  private object CTypes {
    def KindlingsXmlCodec: Type.Ctor1[KindlingsXmlCodec] = Type.Ctor1.of[KindlingsXmlCodec]
  }
}
