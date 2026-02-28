package hearth.kindlings.yamlderivation.internal.compiletime

import hearth.MacroCommons
import hearth.std.*

import hearth.kindlings.yamlderivation.{KindlingsYamlCodec, YamlConfig}
import hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationUtils
import org.virtuslab.yaml.{YamlDecoder, YamlEncoder}

trait CodecMacrosImpl {
  this: MacroCommons & StdExtensions & AnnotationSupport & EncoderMacrosImpl & DecoderMacrosImpl =>

  @scala.annotation.nowarn("msg=is never used")
  def deriveCodecTypeClass[A: Type](configExpr: Expr[YamlConfig]): Expr[KindlingsYamlCodec[A]] = {
    implicit val KindlingsYamlCodecA: Type[KindlingsYamlCodec[A]] = CTypes.KindlingsYamlCodec[A]

    // Step 1: Derive YamlEncoder[A]
    val encoderExpr: Expr[YamlEncoder[A]] =
      deriveEncoderTypeClass[A](configExpr).asInstanceOf[Expr[YamlEncoder[A]]]
    // Step 2: Derive YamlDecoder[A]
    val decoderExpr: Expr[YamlDecoder[A]] =
      deriveDecoderTypeClass[A](configExpr).asInstanceOf[Expr[YamlDecoder[A]]]
    // Step 3: Combine at runtime
    Expr.quote {
      YamlDerivationUtils.yamlCodec[A](
        Expr.splice(encoderExpr),
        Expr.splice(decoderExpr)
      )
    }
  }

  private object CTypes {
    def KindlingsYamlCodec: Type.Ctor1[KindlingsYamlCodec] = Type.Ctor1.of[KindlingsYamlCodec]
  }
}
