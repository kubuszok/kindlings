package hearth.kindlings.yamlderivation

import scala.language.experimental.macros

private[yamlderivation] trait KindlingsYamlCodecCompanionCompat { this: KindlingsYamlCodec.type =>

  @deprecated("Use .derived instead", "next")
  def derive[A](implicit config: YamlConfig): KindlingsYamlCodec[A] =
    macro internal.compiletime.CodecMacros.deriveCodecImpl[A]

  implicit def derived[A](implicit config: YamlConfig): KindlingsYamlCodec[A] =
    macro internal.compiletime.CodecMacros.deriveKindlingsCodecImpl[A]
}
