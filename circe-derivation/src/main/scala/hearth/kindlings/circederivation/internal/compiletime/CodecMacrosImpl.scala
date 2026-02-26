package hearth.kindlings.circederivation.internal.compiletime

import hearth.MacroCommons
import hearth.std.*

import hearth.kindlings.circederivation.{Configuration, KindlingsCodecAsObject, KindlingsEncoderAsObject}
import hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils
import io.circe.Decoder

trait CodecMacrosImpl {
  this: MacroCommons & StdExtensions & AnnotationSupport & EncoderMacrosImpl & DecoderMacrosImpl =>

  @scala.annotation.nowarn("msg=is never used")
  def deriveCodecAsObjectTypeClass[A: Type](configExpr: Expr[Configuration]): Expr[KindlingsCodecAsObject[A]] = {
    implicit val KindlingsCodecAsObjectA: Type[KindlingsCodecAsObject[A]] = CTypes.KindlingsCodecAsObject[A]

    // Step 1: Derive Encoder.AsObject[A]
    val encoderExpr: Expr[KindlingsEncoderAsObject[A]] = deriveEncoderAsObjectTypeClass[A](configExpr)
    // Step 2: Derive Decoder[A]
    val decoderExpr: Expr[Decoder[A]] = deriveDecoderTypeClass[A](configExpr).asInstanceOf[Expr[Decoder[A]]]
    // Step 3: Combine at runtime
    Expr.quote {
      CirceDerivationUtils.codecAsObject[A](
        Expr.splice(encoderExpr),
        Expr.splice(decoderExpr)
      )
    }
  }

  private[compiletime] object CTypes {
    def KindlingsCodecAsObject: Type.Ctor1[KindlingsCodecAsObject] = Type.Ctor1.of[KindlingsCodecAsObject]
  }
}
