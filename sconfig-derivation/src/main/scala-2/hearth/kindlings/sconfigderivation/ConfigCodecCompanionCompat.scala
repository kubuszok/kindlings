package hearth.kindlings.sconfigderivation

import scala.language.experimental.macros

private[sconfigderivation] trait ConfigCodecCompanionCompat { this: ConfigCodec.type =>

  // Scala 2 uses a single macro that combines reader and writer derivation in one
  // expansion (no sibling-splice issue on Scala 2). The Scala 3 implementation uses
  // inline composition instead.
  def derive[A](implicit config: SConfig): ConfigCodec[A] =
    macro internal.compiletime.CodecMacros.deriveCodecImpl[A]

  def derived[A](implicit config: SConfig): ConfigCodec[A] =
    macro internal.compiletime.CodecMacros.deriveCodecImpl[A]
}
