package hearth.kindlings.sconfigderivation

import scala.language.experimental.macros

private[sconfigderivation] trait ConfigCodecCompanionCompat { this: ConfigCodec.type =>

  def derived[A](implicit config: SConfig): ConfigCodec[A] =
    macro internal.compiletime.CodecMacros.deriveCodecImpl[A]
}
