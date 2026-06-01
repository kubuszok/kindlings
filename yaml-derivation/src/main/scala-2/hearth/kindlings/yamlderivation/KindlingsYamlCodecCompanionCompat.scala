package hearth.kindlings.yamlderivation

import scala.language.experimental.macros

private[yamlderivation] trait KindlingsYamlCodecCompanionCompat { this: KindlingsYamlCodec.type =>

  implicit def derived[A](implicit config: YamlConfig): KindlingsYamlCodec[A] =
    macro internal.compiletime.CodecMacros.deriveKindlingsCodecImpl[A]
}
