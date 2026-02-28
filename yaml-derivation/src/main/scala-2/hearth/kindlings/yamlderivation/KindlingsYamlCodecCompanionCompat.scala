package hearth.kindlings.yamlderivation

import scala.language.experimental.macros

private[yamlderivation] trait KindlingsYamlCodecCompanionCompat { this: KindlingsYamlCodec.type =>

  def derive[A](implicit config: YamlConfig): KindlingsYamlCodec[A] =
    macro internal.compiletime.CodecMacros.deriveCodecImpl[A]

  implicit def derived[A](implicit config: YamlConfig): KindlingsYamlCodec[A] =
    macro internal.compiletime.CodecMacros.deriveKindlingsCodecImpl[A]
}
