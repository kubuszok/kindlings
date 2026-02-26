package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonCodec, JsonKeyCodec}

private[jsoniterderivation] trait KindlingsJsonCodecCompanionCompat { this: KindlingsJsonCodec.type =>

  inline def derive[A](using config: JsoniterConfig): JsonCodec[A] =
    internal.runtime.JsoniterDerivationUtils.jsonCodec[A](
      KindlingsJsonValueCodec.derive[A],
      deriveKeyCodec[A]
    )

  inline def deriveKeyCodec[A](using config: JsoniterConfig): JsonKeyCodec[A] = ${
    internal.compiletime.CodecMacros.deriveKeyCodecImpl[A]('config)
  }

  inline given derived[A](using config: JsoniterConfig): KindlingsJsonCodec[A] =
    internal.runtime.JsoniterDerivationUtils
      .jsonCodec[A](
        KindlingsJsonValueCodec.derive[A],
        deriveKeyCodec[A]
      )
      .asInstanceOf[KindlingsJsonCodec[A]]
}
